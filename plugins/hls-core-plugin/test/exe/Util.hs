{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}
{-# LANGUAGE DataKinds         #-}

module Util where

import           Control.Applicative         ((<|>))
import           Control.Arrow               (Arrow (..))
import           Control.Lens                (_1, traverseOf, (^.))
import           Control.Monad               (unless, void, (>=>))
import           Control.Monad.IO.Class      (MonadIO (..))
import qualified Data.Aeson                  as A
import           Data.Data                   (Proxy (..))
import           Data.Default                (Default (..))
import           Data.Foldable               (traverse_)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text                   as Text
import           Debug.Trace                 (traceShow)
import           Development.IDE             (GhcVersion, ghcVersion)
import           Development.IDE.Plugin.Test (TestRequest (..))
import           GHC.Stack                   (HasCallStack)
import           GHC.TypeLits                (symbolVal)
import qualified Ide.Plugin.Core             as Core
import           Ide.Types                   (Config (..))
import           Language.LSP.Protocol.Lens  (HasRange (..), HasStart (..),
                                              HasTags (..))
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types (Definition (..),
                                              DefinitionLink (..), Diagnostic,
                                              DiagnosticSeverity, DiagnosticTag,
                                              Location (..), LocationLink (..),
                                              Null (..), Position (..),
                                              Range (..), UInt, Uri (..),
                                              filePathToUri, mkRange,
                                              toNormalizedUri,
                                              type (|?) (InL, InR),
                                              uriToFilePath)
import           Language.LSP.Test           (Session, sendRequest)
import qualified Language.LSP.Test           as LspTest
import           System.Directory.Extra      (canonicalizePath)
import           System.FilePath             (equalFilePath, (</>))
import           System.Info.Extra
import           System.Time.Extra           (Seconds, sleep)
import           Test.Hls                    (FromServerMessage' (..),
                                              Method (Method_TextDocumentPublishDiagnostics),
                                              NormalizedUri,
                                              PluginTestDescriptor,
                                              SMethod (..), TCustomMessage (..),
                                              TNotificationMessage (..),
                                              TServerMessage, TestName,
                                              TestRunner, TestTree, assertBool,
                                              expectFailBecause, getDocUri,
                                              ignoreTestBecause,
                                              mkPluginTestDescriptor,
                                              runSessionWithServer,
                                              runSessionWithServerInTmpDir,
                                              satisfyMaybe, setConfigSection,
                                              skipManyTill, testCase)
import qualified Test.Hls.FileSystem         as FS
import           Test.Hls.FileSystem         (copy, file, text)
import           Test.Tasty.HUnit            (Assertion, assertFailure, (@=?),
                                              (@?=))


pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

testSessionWithCorePlugin ::(TestRunner cont ()) => TestName -> FS.VirtualFileTree -> cont -> TestTree
testSessionWithCorePlugin caseName vfs = testCase caseName . runSessionWithCorePlugin vfs

testSessionWithCorePluginEmptyVsf ::(TestRunner cont ()) => TestName -> cont -> TestTree
testSessionWithCorePluginEmptyVsf caseName = testSessionWithCorePlugin caseName (mkFs [])

runSessionWithCorePluginNoVsf :: Session a -> IO a
runSessionWithCorePluginNoVsf = runSessionWithCorePlugin (mkFs [])

testSessionWithCorePluginSubDir ::(TestRunner cont ()) => TestName -> FilePath -> cont -> TestTree
testSessionWithCorePluginSubDir caseName dir = testSessionWithCorePlugin caseName (mkFs [FS.copyDir dir])

runSessionWithCorePlugin :: (TestRunner cont res) => FS.VirtualFileTree -> cont -> IO res
runSessionWithCorePlugin = runSessionWithServerInTmpDir def corePlugin

runSessionWithServerCorePlugin :: FilePath -> Session a -> IO a
runSessionWithServerCorePlugin = runSessionWithServer def corePlugin

runSessionWithCorePluginEmpty :: [Text] -> Session a -> IO a
runSessionWithCorePluginEmpty fps = runSessionWithCorePlugin (mkFs [FS.directCradle fps])

runSessionWithCorePluginSingleFile :: FilePath -> Text -> Session a -> IO a
runSessionWithCorePluginSingleFile fp content = runSessionWithCorePlugin (mkSingleFileFs fp content)

runSessionWithCorePluginSingleDirFile :: FilePath -> FilePath -> Session a -> IO a
runSessionWithCorePluginSingleDirFile dir fp = runSessionWithCorePlugin (mkSingleDirFileFs dir fp)

testSessionWithCorePluginSingleFile :: TestName -> FilePath -> Text -> Session () -> TestTree
testSessionWithCorePluginSingleFile caseName fp content = testCase caseName . runSessionWithCorePluginSingleFile fp content

testSessionWithCorePluginSingleDirFile :: TestName
    -> FilePath -- ^ subDir under testDataDir
    -> FilePath -- ^ fileName
    -> Session () -> TestTree
testSessionWithCorePluginSingleDirFile caseName subDir fp = testCase caseName . runSessionWithCorePluginSingleDirFile subDir fp

corePlugin :: PluginTestDescriptor Core.CoreLog
corePlugin = mkPluginTestDescriptor Core.descriptor "core"

mkSingleFileFs :: FilePath -> Text -> FS.VirtualFileTree
mkSingleFileFs fp = mkFs . directFile fp

mkSingleDirFileFs :: FilePath -> FilePath -> FS.VirtualFileTree
mkSingleDirFileFs dir fp = FS.mkVirtualFileTree (testDataDir </> dir) [FS.directCradle [Text.pack fp], copy fp]

directFile :: FilePath -> Text -> [FS.FileTree]
directFile fp content =
  [ FS.directCradle [Text.pack fp]
  , file fp (text content)
  ]

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-core-plugin" </> "test" </> "testdata"


data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
  | ExpectLocation Location
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectHoverExcludeText [T.Text] -- the hover message must _not_ contain these snippets
  | ExpectHoverTextRegex T.Text -- the hover message must match this pattern
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
  | ExpectNoHover
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples
  deriving Eq

defToLocation :: Definition |? ([DefinitionLink] |? Null) -> [Location]
defToLocation (InL (Definition (InL l))) = [l]
defToLocation (InL (Definition (InR ls))) = ls
defToLocation (InR (InL defLink)) = (\(DefinitionLink LocationLink{_targetUri,_targetRange}) -> Location _targetUri _targetRange) <$> defLink
defToLocation (InR (InR Null)) = []

checkDefs :: Definition |? ([DefinitionLink] |? Null) -> Session [Expect] -> Session ()
checkDefs (defToLocation -> defs) mkExpectations = traverse_ check =<< mkExpectations where
  check (ExpectRange expectedRange) = do
    def <- assertOneDefinitionFound defs
    assertRangeCorrect def expectedRange
  check (ExpectLocation expectedLocation) = do
    def <- assertOneDefinitionFound defs
    liftIO $ do
      canonActualLoc <- canonicalizeLocation def
      canonExpectedLoc <- canonicalizeLocation expectedLocation
      canonActualLoc @?= canonExpectedLoc
  check ExpectNoDefinitions = do
    liftIO $ assertBool "Expecting no definitions" $ null defs
  check ExpectExternFail = liftIO $ assertFailure "Expecting to fail to find in external file"
  check _ = pure () -- all other expectations not relevant to getDefinition

  assertOneDefinitionFound :: [Location] -> Session Location
  assertOneDefinitionFound [def] = pure def
  assertOneDefinitionFound xs = liftIO . assertFailure $ "Expecting exactly one definition, got " <> show (length xs)

  assertRangeCorrect Location{_range = foundRange} expectedRange =
    liftIO $ expectedRange @=? foundRange



canonicalizeLocation :: Location -> IO Location
canonicalizeLocation (Location uri range) = Location <$> canonicalizeUri uri <*> pure range

canonicalizeUri :: Uri -> IO Uri
canonicalizeUri uri = filePathToUri <$> canonicalizePath (fromJust (uriToFilePath uri))


mkR :: UInt -> UInt -> UInt -> UInt -> Expect
mkR startLine startColumn endLine endColumn = ExpectRange $ mkRange startLine startColumn endLine endColumn

mkL :: Uri -> UInt -> UInt -> UInt -> UInt -> Expect
mkL uri startLine startColumn endLine endColumn = ExpectLocation $ Location uri $ mkRange startLine startColumn endLine endColumn

-- mkRange :: UInt -> UInt -> UInt -> UInt -> Range
-- mkRange a b c d = Range (Position a b) (Position c d)

xfail :: TestTree -> String -> TestTree
xfail = flip expectFailBecause

standardizeQuotes :: T.Text -> T.Text
standardizeQuotes msg = let
        repl '‘' = '\''
        repl '’' = '\''
        repl '`' = '\''
        repl  c  = c
    in  T.map repl msg


configureCheckProject :: Bool -> Session ()
configureCheckProject overrideCheckProject = setConfigSection "haskell" (A.toJSON $ def{checkProject = overrideCheckProject})


referenceReady :: (FilePath -> Bool) -> Session FilePath
referenceReady pred = satisfyMaybe $ \case
  FromServerMess (SMethod_CustomMethod p) (NotMess TNotificationMessage{_params})
    | A.Success fp <- A.fromJSON _params
    , pred fp
    , symbolVal p == "ghcide/reference/ready"
    -> traceShow ("referenceReady", fp) $ Just fp
  _ -> Nothing

-- | Pattern match a message from ghcide indicating that a file has been indexed
isReferenceReady :: FilePath -> Session ()
isReferenceReady p = void $ referenceReady (equalFilePath p)

-- |wait for @timeout@ seconds and report an assertion failure
-- if any diagnostic messages arrive in that period
-- expectNoMoreDiagnostics :: (HasCallStack) => Seconds -> Session ()
-- expectNoMoreDiagnostics timeout =
--   expectMessages SMethod_TextDocumentPublishDiagnostics timeout $ \diagsNot -> do
--     let fileUri = diagsNot ^. L.params . L.uri
--         actual = diagsNot ^. L.params . L.diagnostics
--     unless (actual == []) $ liftIO $
--       assertFailure $
--         "Got unexpected diagnostics for " <> show fileUri
--           <> " got "
--           <> show actual

expectMessages :: SMethod m -> Seconds -> (TServerMessage m -> Session ()) -> Session ()
expectMessages m timeout handle = do
    -- Give any further diagnostic messages time to arrive.
    liftIO $ sleep timeout
    -- Send a dummy message to provoke a response from the server.
    -- This guarantees that we have at least one message to
    -- process, so message won't block or timeout.
    let cm = SMethod_CustomMethod (Proxy @"test")
    i <- sendRequest cm $ A.toJSON GetShakeSessionQueueCount
    go cm i
  where
    go cm i = handleMessages
      where
        handleMessages = (LspTest.message m >>= handle) <|> (void $ LspTest.responseForId cm i) <|> ignoreOthers
        ignoreOthers = void LspTest.anyMessage >> handleMessages

type Cursor = (UInt, UInt)
-- | It is not possible to use 'expectDiagnostics []' to assert the absence of diagnostics,
--   only that existing diagnostics have been cleared.
--
--   Rather than trying to assert the absence of diagnostics, introduce an
--   expected diagnostic (e.g. a redundant import) and assert the singleton diagnostic.
expectDiagnostics :: (HasCallStack) => [(FilePath, [(DiagnosticSeverity, Cursor, T.Text)])] -> Session ()
expectDiagnostics = expectDiagnosticsWithTags . map (second (map (\(ds, c, t) -> (ds, c, t, Nothing))))

unwrapDiagnostic :: TServerMessage Method_TextDocumentPublishDiagnostics  -> (Uri, [Diagnostic])
unwrapDiagnostic diagsNot = (diagsNot^. L.params . L.uri, diagsNot^. L.params . L.diagnostics)
expectDiagnosticsWithTags :: HasCallStack => [(String, [(DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)])] -> Session ()
expectDiagnosticsWithTags expected = do
    let f = getDocUri >=> liftIO . canonicalizeUri >=> pure . toNormalizedUri
        next = unwrapDiagnostic <$> skipManyTill LspTest.anyMessage diagnostic
    expected' <- Map.fromListWith (<>) <$> traverseOf (traverse . _1) f expected
    expectDiagnosticsWithTags' next expected'

diagnostic :: Session (TNotificationMessage Method_TextDocumentPublishDiagnostics)
diagnostic = LspTest.message SMethod_TextDocumentPublishDiagnostics

expectDiagnosticsWithTags' ::
  (HasCallStack, MonadIO m) =>
  m (Uri, [Diagnostic]) ->
  Map.Map NormalizedUri [(DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)] ->
  m ()
expectDiagnosticsWithTags' next m | null m = do
    (_,actual) <- next
    case actual of
        [] ->
            return ()
        _ ->
            liftIO $ assertFailure $ "Got unexpected diagnostics:" <> show actual

expectDiagnosticsWithTags' next expected = go expected
  where
    go m
      | Map.null m = pure ()
      | otherwise = do
        (fileUri, actual) <- next
        canonUri <- liftIO $ toNormalizedUri <$> canonicalizeUri fileUri
        case Map.lookup canonUri m of
          Nothing -> do
            liftIO $
              assertFailure $
                "Got diagnostics for " <> show fileUri
                  <> " but only expected diagnostics for "
                  <> show (Map.keys m)
                  <> " got "
                  <> show actual
          Just expected -> do
            liftIO $ mapM_ (requireDiagnosticM actual) expected
            liftIO $
              unless (length expected == length actual) $
                assertFailure $
                  "Incorrect number of diagnostics for " <> show fileUri
                    <> ", expected "
                    <> show expected
                    <> " but got "
                    <> show actual
            go $ Map.delete canonUri m

-- expectCurrentDiagnostics :: HasCallStack => TextDocumentIdentifier -> [(DiagnosticSeverity, Cursor, T.Text)] -> Session ()
-- expectCurrentDiagnostics doc expected = do
--     diags <- getCurrentDiagnostics doc
--     checkDiagnosticsForDoc doc expected diags

-- checkDiagnosticsForDoc :: HasCallStack => TextDocumentIdentifier -> [(DiagnosticSeverity, Cursor, T.Text)] -> [Diagnostic] -> Session ()
-- checkDiagnosticsForDoc TextDocumentIdentifier {_uri} expected obtained = do
--     let expected' = Map.singleton nuri (map (\(ds, c, t) -> (ds, c, t, Nothing)) expected)
--         nuri = toNormalizedUri _uri
--     expectDiagnosticsWithTags' (return (_uri, obtained)) expected'


-- diagnostic :: Session (TNotificationMessage Method_TextDocumentPublishDiagnostics)
-- diagnostic = LspTest.message SMethod_TextDocumentPublishDiagnostics


requireDiagnosticM
    :: (Foldable f, Show (f Diagnostic), HasCallStack)
    => f Diagnostic
    -> (DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)
    -> Assertion
requireDiagnosticM actuals expected = case requireDiagnostic actuals expected of
    Nothing  -> pure ()
    Just err -> assertFailure err

type ErrorMsg = String

cursorPosition :: Cursor -> Position
cursorPosition (line,  col) = Position line col

requireDiagnostic
    :: (Foldable f, Show (f Diagnostic), HasCallStack)
    => f Diagnostic
    -> (DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)
    -> Maybe ErrorMsg
requireDiagnostic actuals expected@(severity, cursor, expectedMsg, expectedTag)
    | any match actuals = Nothing
    | otherwise = Just $
            "Could not find " <> show expected <>
            " in " <> show actuals
  where
    match :: Diagnostic -> Bool
    match d =
        Just severity ==  d ^. L.severity
        && cursorPosition cursor == d ^. range . start
        && standardizeQuotes (T.toLower expectedMsg) `T.isInfixOf`
           standardizeQuotes (T.toLower $ d ^. L.message)
        && hasTag expectedTag (d ^. tags)

    hasTag :: Maybe DiagnosticTag -> Maybe [DiagnosticTag] -> Bool
    hasTag Nothing  _                   = True
    hasTag (Just _) Nothing             = False
    hasTag (Just actualTag) (Just tags) = actualTag `elem` tags




