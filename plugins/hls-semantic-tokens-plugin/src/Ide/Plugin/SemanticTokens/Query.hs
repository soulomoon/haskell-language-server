-- |
-- The query module is used to query the semantic tokens from the AST
module Ide.Plugin.SemanticTokens.Query where

import           Control.Applicative                  ((<|>))
import           Data.Data                            (Data)
import           Data.Foldable                        (fold)
import           Data.Generics                        (mkQ)
import           Data.Generics.Aliases                (extQ)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (listToMaybe, mapMaybe)
import qualified Data.Set                             as S
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentRange)
import           Development.IDE.GHC.Compat
import           Generics.SYB                         (everything)
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types      (HieFunMaskKind,
                                                       HsLTokenSet, HsLTokens,
                                                       HsSemanticTokenType (TModule),
                                                       RangeSemanticTokenTypeList,
                                                       SemanticTokenId,
                                                       SemanticTokensConfig)
import           Language.LSP.Protocol.Types          (Position (Position),
                                                       Range (Range),
                                                       SemanticTokenAbsolute (SemanticTokenAbsolute),
                                                       SemanticTokens (SemanticTokens),
                                                       SemanticTokensDelta (SemanticTokensDelta),
                                                       defaultSemanticTokensLegend,
                                                       makeSemanticTokens,
                                                       makeSemanticTokensDelta)
import           Prelude                              hiding (length, span)

---------------------------------------------------------

-- * extract occ names and range

---------------------------------------------------------

getLoc1 :: GenLocated l e -> l
getLoc1 (L l _) = l


getAdornment :: SrcSpanAnn' (EpAnn NameAnn) -> Maybe NameAdornment
getAdornment sp =
    case ann sp of
                            EpAnnNotUsed ->  Nothing
                            x -> case anns x of
                                (NameAnn adornament _ _ _ _) -> case adornament of
                                    NameParens     -> Just NameParens
                                    NameBackquotes -> Just NameBackquotes
                                    NameParensHash -> Nothing
                                    NameSquare     -> Nothing
                                _ -> Nothing
nameGetterPs :: Data a => a -> HsLTokenSet
nameGetterPs = everything (<>) (mempty `mkQ` getName' `extQ` getModuleName' `extQ` getDocName)
    where
    getName' :: LIdP GhcPs -> HsLTokenSet
    getName' idn = case getSpan $ getLoc1 idn of
        Just sp ->  S.fromList [((sp, getAdornment $ getLoc1 idn), Left (unLoc idn))]
        Nothing -> mempty
    getDocName :: Located (IdP GhcPs) -> HsLTokenSet
    getDocName idn = case srcSpanToRealSrcSpan $ getLoc1 idn of
        Just sp -> S.fromList [ ((sp, Nothing), Left (unLoc idn))]
        Nothing -> mempty
    getModuleName' :: GenLocated SrcSpanAnnA ModuleName -> HsLTokenSet
    getModuleName' idn =
        case getSpan $ getLoc1 idn of
            Just sp -> S.fromList [((sp, Nothing), Right (unLoc idn))]
            Nothing -> mempty
mkSrcSpanAnn' :: SrcSpan -> SrcSpanAnn' (EpAnn ann)
mkSrcSpanAnn' sp = SrcSpanAnn {ann = EpAnnNotUsed, locA = sp}


-- getSpan :: HsLToken -> Maybe RealSrcSpan
-- getSpan :: SrcSpanAnn' a -> Maybe RealSrcSpan
getSpan :: SrcSpanAnn' a -> Maybe RealSrcSpan
getSpan sp = srcSpanToRealSrcSpan $ locA sp

srcSpanToRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
srcSpanToRealSrcSpan (RealSrcSpan ss _) = Just ss
srcSpanToRealSrcSpan _                  = Nothing



---------------------------------------------------------

-- * extract semantic

---------------------------------------------------------

idSemantic :: forall a. NameEnv TyThing -> HieFunMaskKind a -> RefMap a -> Identifier -> Maybe HsSemanticTokenType
idSemantic _ _ _ (Left _) = Just TModule
idSemantic tyThingMap hieKind rm (Right n) =
    nameSemanticFromHie hieKind rm n -- local name
    <> (lookupNameEnv tyThingMap n >>= tyThingSemantic) -- global name
    -- nameSemanticFromHie hieKind rm n -- local name
    -- <|> (lookupNameEnv tyThingMap n >>= tyThingSemantic) -- global name


---------------------------------------------------------

-- * extract semantic from HieAst for local variables

---------------------------------------------------------

nameSemanticFromHie :: forall a. HieFunMaskKind a -> RefMap a -> Name -> Maybe HsSemanticTokenType
nameSemanticFromHie hieKind rm n = idSemanticFromRefMap rm (Right n)
  where
    idSemanticFromRefMap :: RefMap a -> Identifier -> Maybe HsSemanticTokenType
    idSemanticFromRefMap rm' name' = do
      spanInfos <- M.lookup name' rm'
      let typeTokenType = foldMap (typeSemantic hieKind) $ listToMaybe $ mapMaybe (identType . snd) spanInfos
      contextInfoTokenType <- foldMap (contextInfosMaybeTokenType . identInfo . snd) spanInfos
      fold [typeTokenType, Just contextInfoTokenType, nameInfixOperator n]

    contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe HsSemanticTokenType
    contextInfosMaybeTokenType details = foldMap infoTokenType (Set.toList details)


-------------------------------------------------

-- * extract lsp semantic tokens from RangeSemanticTokenTypeList

-------------------------------------------------

rangeSemanticsSemanticTokens :: SemanticTokenId -> SemanticTokensConfig -> PositionMapping -> RangeSemanticTokenTypeList -> Either Text SemanticTokens
rangeSemanticsSemanticTokens sid stc mapping =
  makeSemanticTokensWithId (Just sid) . mapMaybe (\(ran, tk) -> toAbsSemanticToken <$> toCurrentRange mapping ran <*> return tk)
  where
    toAbsSemanticToken :: Range -> HsSemanticTokenType -> SemanticTokenAbsolute
    toAbsSemanticToken (Range (Position startLine startColumn) (Position _endLine endColumn)) tokenType =
      let len = endColumn - startColumn
       in SemanticTokenAbsolute
            (fromIntegral startLine)
            (fromIntegral startColumn)
            (fromIntegral len)
            (toLspTokenType stc tokenType)
            []

makeSemanticTokensWithId :: Maybe SemanticTokenId -> [SemanticTokenAbsolute] -> Either Text SemanticTokens
makeSemanticTokensWithId sid tokens = do
    (SemanticTokens _  tokens) <- makeSemanticTokens defaultSemanticTokensLegend tokens
    return $ SemanticTokens sid tokens

makeSemanticTokensDeltaWithId :: Maybe SemanticTokenId ->  SemanticTokens -> SemanticTokens -> SemanticTokensDelta
makeSemanticTokensDeltaWithId sid previousTokens currentTokens =
    let (SemanticTokensDelta _ stEdits) = makeSemanticTokensDelta previousTokens currentTokens
    in SemanticTokensDelta sid stEdits

