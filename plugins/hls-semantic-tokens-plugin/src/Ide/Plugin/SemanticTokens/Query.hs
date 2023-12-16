{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ide.Plugin.SemanticTokens.Query where
import           Control.Arrow                       (ArrowChoice ((|||)),
                                                      (&&&))
import           Control.Monad                       (forM)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Char                           (isAlphaNum)
import           Data.Either                         (rights)
import           Data.Function                       (on)
import           Data.Generics                       (everything)
import qualified Data.HashSet                        as HashSet
import qualified Data.List                           as List
import qualified Data.List.NonEmpty                  as NE
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (catMaybes, fromMaybe,
                                                      listToMaybe, mapMaybe)
import           Data.Ord                            (comparing)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Lazy.Builder              as Text
import           Data.Tuple                          (swap)
import           Debug.Trace
import           Development.IDE                     (Action, filePathToUri',
                                                      rangeToRealSrcSpan,
                                                      realSpan)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error           (positionToRealSrcLoc)
import           Development.IDE.Spans.AtPoint       (FOIReferences)
import           Development.IDE.Spans.LocalBindings (Bindings)
import           Development.IDE.Types.Exports       (ExportsMap (getModuleExportsMap),
                                                      IdentInfo)
import           Development.IDE.Types.Location      (toNormalizedFilePath')
import           Development.IDE.Types.Shake         (WithHieDb)
import           Generics.SYB                        (mkQ)
-- import HieDb.Types (DefRow (..))
import           HieDb                               (DefRow (..),
                                                      HieDbErr (AmbiguousUnitId, NameNotFound, NameUnhelpfulSpan, NoNameAtPoint, NotIndexed),
                                                      ModuleInfo (modInfoSrcFile),
                                                      RefRow (..), Res,
                                                      findOneDef,
                                                      findReferences,
                                                      type (:.) (..))
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types
import           Language.LSP.Protocol.Types


------------------------------------------------
--- | extract semantic tokens from RenamedSource
------------------------------------------------

nameToCollect :: Name -> NameSet
nameToCollect = unitNameSet

nameGetter :: RenamedSource -> NameSet
nameGetter =  everything unionNameSet (emptyNameSet `mkQ` nameToCollect)

--------------------------------------------
--- | construct definition map from HieAST a
--------------------------------------------

hieAstSpanNames :: NameSet -> HieAST a -> [(Span, Name)]
hieAstSpanNames nameSet ast = if null (nodeChildren ast) then
    getIds ast else concatMap (hieAstSpanNames nameSet) (nodeChildren ast)
    where
        getIds :: HieAST a -> [(Span, Name)]
        getIds ast = [(nodeSpan ast, c)
                    | (Right c, d) <- Map.toList $ getNodeIds' ast
                    , elemNameSet c nameSet
                    -- at least get one info
                    , not $ any isEvidenceBind $ identInfo d
                    , not $ any isEvidenceUse $ identInfo d
                    , Set.size (identInfo d) > 0
                    -- some derived occName is visible (some fields)
                    -- , not $ isDerivedOccName (occName c)
                    ]
        getNodeIds' :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
        getNodeIds' = Map.foldl' combineNodeIds Map.empty
            .  Map.filterWithKey (\k _ -> k == SourceInfo)
            . getSourcedNodeInfo . sourcedNodeInfo

        combineNodeIds :: Map.Map Identifier (IdentifierDetails a)
                                -> NodeInfo a -> Map.Map Identifier (IdentifierDetails a)
        -- ad `combineNodeIds` (NodeInfo SourceInfo _ bd) = bd
        ad `combineNodeIds` (NodeInfo _ _ bd) = Map.unionWith (<>) ad bd


--------------------------------------------------
--- | extract semantic tokens from NameSemanticMap
--------------------------------------------------


toNameSemanticMap :: NameSet -> RefMap a -> NameSemanticMap
toNameSemanticMap ns rm = extendNameEnvList_C (<>) emptyNameEnv
    [
    --  trace ("toNameSemanticMap" <> ":" <> showSDocUnsafe (ppr name) <> " : " <> showCompactRealSrc span <> ":" <> showIdentifierDetails detail <> " : " <> show tokenType)
     (name, tokenType)
    | (name, Just details) <- map (\x -> (x, Map.lookup (Right x) rm)) $ nameSetElemsStable ns
    , not $ isDerivedOccName (occName name)
    , (span, detail) <- details
    , let tokenType =  detailSemanticMaybeTokenType $ identInfo detail
    , (Just tokenType) <- [tokenType]
    ]
    where
        detailSemanticMaybeTokenType ::  Set.Set ContextInfo -> Maybe SemanticTokenType
        detailSemanticMaybeTokenType details = case NE.nonEmpty $ Set.toList details of
            Just infos -> Just $ maximum $ NE.map infoTokenType infos
            Nothing    -> Nothing

semanticTokenAbsoluteSemanticTokens :: [SemanticTokenAbsolute] -> Either Text SemanticTokens
semanticTokenAbsoluteSemanticTokens = makeSemanticTokens defaultSemanticTokensLegend . List.sort

extractSemanticTokensFromNames :: NameSemanticMap -> [(Span, Name)] -> [SemanticTokenAbsolute]
extractSemanticTokensFromNames nsm =
    mapMaybe (uncurry toAbsSemanticToken) . mergeNameFromSamSpan . mapMaybe (getSemantic nsm)
    where
        -- merge all tokens with same span
        mergeNameFromSamSpan :: [(Span, SemanticTokenType)] -> [(Span, SemanticTokenType)]
        mergeNameFromSamSpan xs = Map.toList $ Map.fromListWith (<>) xs

        toAbsSemanticToken :: Span -> SemanticTokenType -> Maybe SemanticTokenAbsolute
        toAbsSemanticToken loc tokenType =
            let line = srcSpanStartLine loc - 1
                startChar = srcSpanStartCol loc - 1
                len = srcSpanEndCol loc - 1 - startChar
            in SemanticTokenAbsolute (fromIntegral line) (fromIntegral startChar)
                (fromIntegral len) <$> toLspTokenType tokenType <*> return []
                -- SemanticTokenModifiers_Declaration

        getSemantic :: NameSemanticMap -> (Span, Name) -> Maybe (Span, SemanticTokenType)
        getSemantic nameMap (span, name) = do
            -- let tkt = toTokenType name
            -- let tokenType = maybe tkt (\x -> tkt <> x) $ Map.lookup name nameMap
            let tokenType = lookupNameEnv nameMap name
            fmap (span,) tokenType


