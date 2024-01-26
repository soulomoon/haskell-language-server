
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}

module Ide.Plugin.SemanticTokens.Tokenize (hieAstSpanIdentifiers) where

import           Control.Lens                    (Identity (runIdentity))
import           Control.Monad                   (forM_, guard)
import           Control.Monad.State             (MonadState (get),
                                                  MonadTrans (lift), execStateT,
                                                  gets, modify, put)
import           Control.Monad.Trans.State       (StateT)
import qualified Data.Map                        as M
import qualified Data.Map                        as Map
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Rope                  as Char
import           Data.Text.Utf16.Rope            (toText)
import qualified Data.Text.Utf16.Rope            as Utf16
import           Data.Text.Utf16.Rope.Mixed      (Rope)
import qualified Data.Text.Utf16.Rope.Mixed      as Rope
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error       (realSrcSpanToCodePointRange)
import           Ide.Plugin.SemanticTokens.Types (RangeIdSetMap)
import           Language.LSP.Protocol.Types     (Position (Position),
                                                  Range (Range), UInt, mkRange)
import           Language.LSP.VFS                hiding (line)
import           Prelude                         hiding (length, span)

type Tokenizer m a = forall t. StateT (PTokenState t) m a


data PTokenState t = PTokenState
  { rangeIdSetMap       :: RangeIdSetMap,
    rope                :: Rope,
    cursor              :: Char.Position,
    columnsInUtf16      :: UInt,
    currentRange        :: Range,
    currentRangeContext :: SplitResult
  }

runTokenizer :: (Monad m) => Tokenizer m a -> PTokenState t -> m RangeIdSetMap
runTokenizer p st = rangeIdSetMap <$> execStateT p st

data SplitResult
  = NoSplit (Text, Range) -- does not need to split, token text, token range
  | Split (Text, Range, Range) -- token text, prefix range(module range), token range
  deriving (Show)

startRange :: Range
startRange = Range (Position 0 0) (Position 0 0)

mkPTokenState :: VirtualFile -> PTokenState a
mkPTokenState vf =
  PTokenState
    { rangeIdSetMap = mempty,
      rope = Rope.fromText $ toText vf._file_text,
      cursor = Char.Position 0 0,
      columnsInUtf16 = 0,
      currentRange = startRange,
      currentRangeContext = NoSplit ("", startRange)
    }

addRangeIdSetMap :: (Monad m) => Range -> Identifier -> Tokenizer m ()
addRangeIdSetMap r i = modify $ \s -> s {rangeIdSetMap = Map.insertWith (<>) r (S.singleton i) $ rangeIdSetMap s}

-- lift a Tokenizer Maybe () to Tokenizer m (),
-- if the Maybe is Nothing, do nothing, recover the state
-- if the Maybe is Just (), do the action, and keep the state
liftMaybeM :: (Monad m) => Tokenizer Maybe () -> Tokenizer m ()
liftMaybeM p = do
  st <- get
  forM_ (execStateT p st) put

hieAstSpanIdentifiers :: VirtualFile -> HieAST a -> RangeIdSetMap
hieAstSpanIdentifiers vf ast = runIdentity $ runTokenizer (foldAst ast) (mkPTokenState vf)

-- | foldAst
-- visit every leaf node in the ast in depth first order
foldAst :: (Monad m) => HieAST t -> Tokenizer m ()
foldAst ast = if null (nodeChildren ast)
  then liftMaybeM (visitLeafIds ast)
  else mapM_ foldAst $ nodeChildren ast

visitLeafIds :: HieAST t -> Tokenizer Maybe ()
visitLeafIds leaf = liftMaybeM $ do
  let span = nodeSpan leaf
  (ran, token) <- focusTokenAt leaf
  -- if `focusTokenAt` succeed, we can safely assume we have shift the cursor correctly
  -- we do not need to recover the cursor state, even if the following computation failed
  liftMaybeM $ do
    -- only handle the leaf node with single column token
    guard $ srcSpanStartLine span == srcSpanEndLine span
    splitResult <-  lift $ splitRangeByText token ran
    modify $ \s -> s {currentRange = ran, currentRangeContext = splitResult}
    mapM_ combineNodeIds $ Map.filterWithKey (\k _ -> k == SourceInfo) $ getSourcedNodeInfo $ sourcedNodeInfo leaf
  where
    combineNodeIds :: (Monad m) => NodeInfo a -> Tokenizer m ()
    combineNodeIds (NodeInfo _ _ bd) = mapM_ getIdentifier (M.keys bd)
    getIdentifier :: (Monad m) => Identifier -> Tokenizer m ()
    getIdentifier idt = liftMaybeM $ do
      ran <- gets currentRange
      case idt of
        Left _moduleName -> addRangeIdSetMap ran idt
        Right name -> do
          ranSplit <- gets currentRangeContext
          occStr <- lift $ case (occNameString . nameOccName) name of
            -- the generated selector name with {-# LANGUAGE DuplicateRecordFields #-}
            '$' : 's' : 'e' : 'l' : ':' : xs -> Just $ takeWhile (/= ':') xs
            ['$']                            -> Just "$"
            -- other generated names that should not be visible
            '$' : _                          -> Nothing
            ns                               -> Just ns
          case ranSplit of
            (NoSplit (tk, r)) -> do
              guard $ T.unpack tk == occStr
              addRangeIdSetMap r idt
            (Split (tk, r1, r2)) -> do
              guard $ T.unpack tk == occStr
              addRangeIdSetMap r1 (Left $ mkModuleName "")
              addRangeIdSetMap r2 idt

focusTokenAt ::
  -- | leaf node we want to focus on
  HieAST a ->
  -- | (token, remains)
  Tokenizer Maybe (Range, Text)
focusTokenAt leaf = do
  PTokenState{cursor, rope, columnsInUtf16} <- get
  let span = nodeSpan leaf
  let (startPos, endPos) = srcSpanCharPositions span
  startOff <- lift $ startPos `sub` cursor
  tokenOff <- lift $ endPos `sub` startPos
  (gap, startRope) <- lift $ charSplitAtPositionMaybe startOff rope
  (token, remains) <- lift $ charSplitAtPositionMaybe tokenOff startRope
  let ncs = newColumn columnsInUtf16 gap
  let nce = newColumn ncs token
  -- compute the new range for utf16, tuning the columns is enough
  let ran = codePointRangeToRangeWith ncs nce $ realSrcSpanToCodePointRange span
  modify $ \s -> s {columnsInUtf16 = nce, rope = remains, cursor = endPos}
  return (ran, token)
  where
    srcSpanCharPositions :: RealSrcSpan -> (Char.Position, Char.Position)
    srcSpanCharPositions real =
        ( realSrcLocRopePosition $ realSrcSpanStart real,
          realSrcLocRopePosition $ realSrcSpanEnd real
        )
    charSplitAtPositionMaybe :: Char.Position -> Rope -> Maybe (Text, Rope)
    charSplitAtPositionMaybe tokenOff rpe = do
      let (prefix, suffix) = Rope.charSplitAtPosition tokenOff rpe
      guard $ Rope.charLengthAsPosition prefix == tokenOff
      return (Rope.toText prefix, suffix)
    sub :: Char.Position -> Char.Position -> Maybe Char.Position
    sub (Char.Position l1 c1) (Char.Position l2 c2)
      | l1 == l2 && c1 > c2 = Just $ Char.Position 0 (c1 - c2)
      | l1 > l2 = Just $ Char.Position (l1 - l2) c1
      | otherwise = Nothing
    realSrcLocRopePosition :: RealSrcLoc -> Char.Position
    realSrcLocRopePosition real = Char.Position (fromIntegral $ srcLocLine real - 1) (fromIntegral $ srcLocCol real - 1)
    -- | newColumn
    -- rope do not treat single \n in our favor
    -- for example, the row length of "123\n" and "123" are both 1
    -- we are forced to use text to compute new column
    newColumn :: UInt -> Text -> UInt
    newColumn n rp = case T.breakOnEnd "\n" rp of
      ("", nEnd) -> n + utf16Length nEnd
      (_, nEnd)  -> utf16Length nEnd
    codePointRangeToRangeWith :: UInt -> UInt -> CodePointRange -> Range
    codePointRangeToRangeWith newStartCol newEndCol (CodePointRange (CodePointPosition startLine _) (CodePointPosition endLine _)) =
      Range (Position startLine newStartCol) (Position endLine newEndCol)

-- | splitRangeByText
-- split a qualified identifier into module name and identifier and/or strip the (), ``
-- for `ModuleA.b`, break it into `ModuleA.` and `b`
-- for `(b)`, strip `()`, and get `b`
-- for `(ModuleA.b)`, strip `()` and break it into `ModuleA.` and `b`
splitRangeByText :: Text -> Range -> Maybe SplitResult
splitRangeByText tk ran = do
  let (ran', tk') = case T.uncons tk of
        Just ('(', xs) -> (subOneRange ran, T.takeWhile (/= ')') xs)
        Just ('`', xs) -> (subOneRange ran, T.takeWhile (/= '`') xs)
        _              -> (ran, tk)
  let (prefix, tk'') = T.breakOnEnd "." tk'
  splitRange tk'' (utf16PositionPosition $ Rope.utf16LengthAsPosition $ Rope.fromText prefix) ran'
  where
    splitRange :: Text -> Position -> Range -> Maybe SplitResult
    splitRange tx (Position l c) r@(Range (Position l1 c1) (Position l2 c2))
      | l1 + l > l2 || (l1 + l == l2 && c > c2) = Nothing -- out of range
      | l==0 && c==0 = Just $ NoSplit (tx, r)
      | otherwise = let c' = if l <= 0 then c1+c else c
                    in Just $ Split (tx, mkRange l1 c1 (l1 + l) c', mkRange (l1 + l) c' l2 c2)
    subOneRange :: Range -> Range
    subOneRange (Range (Position l1 c1) (Position l2 c2)) = Range (Position l1 (c1 + 1)) (Position l2 (c2 - 1))
    utf16PositionPosition :: Utf16.Position -> Position
    utf16PositionPosition (Utf16.Position l c) = Position (fromIntegral l) (fromIntegral c)


utf16Length :: Integral i => Text -> i
utf16Length = fromIntegral . Utf16.length . Utf16.fromText