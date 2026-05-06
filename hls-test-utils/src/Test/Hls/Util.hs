{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Hls.Util
  (  -- * Test Capabilities
      codeActionResolveCaps
    , codeActionNoResolveCaps
    , codeActionNoInlayHintsCaps
    , codeActionSupportCaps
    , expectCodeAction
    -- * Environment specifications
    -- for ignoring tests
    , ghcVersion, GhcVersion(..)
    , hostOS, OS(..)
    , matchesCurrentEnv, EnvSpec(..)
    , ignoreForGhcVersions
    , ignoreInEnv
    , onlyRunForGhcVersions
    , knownBrokenOnWindows
    , knownBrokenForGhcVersions
    , knownBrokenInEnv
    , knownBrokenInSpecificEnv
    , onlyWorkForGhcVersions
    -- * Extract code actions
    , fromAction
    , fromCommand
    -- * Session Assertion Helpers
    , dontExpectCodeAction
    , expectDiagnostic
    , expectNoMoreDiagnostics
    , failIfSessionTimeout
    , getCompletionByLabel
    , noLiteralCaps
    , inspectCodeAction
    , inspectCommand
    , inspectDiagnostic
    , inspectDiagnosticAny
    , waitForDiagnosticsFrom
    , waitForDiagnosticsFromSource
    , waitForActionWithDiagnosticsFromDocs
    , waitForExpectedDiagnosticsFromDocs
    , flushMessages
    -- * Temporary directories
    , withCurrentDirectoryInTmp
    , withCurrentDirectoryInTmp'
    , withCanonicalTempDir
    -- * Extract positions from input file.
    , extractCursorPositions
    , mkParameterisedLabel
    , __i
    -- * Diagnostic checking
    , expectDiagnosticsWithTags-- * Extract positions from input file.
    , diagnostic
    , referenceReady
    , expectCurrentDiagnostics
    , checkDiagnosticsForDoc
    , canonicalizeUri
    -- * Extra test plugin calls
    , waitForAction
    , waitForTypecheck
    , callTestPlugin
    , callTestPluginWithDiag
    , callTestPluginWithSMethod
    , expectDiagnosticsEmpty
    , getStoredKeys
    , getInterfaceFilesDir
    , waitForBuildQueue
    , getFilesOfInterest
    , expectDiagnosticsWithTags'
    , expectedDiagnosticWithNothing
    , expectMessages
    , expectDiagnostics
    , filePathTextDocumentIdentifier
    , waitForExpectedDiagnosticsFromFilePath
    , waitForExpectedDiagnosticsFromDocsOne
    , waitForCustomMessage
    , waitForGC
    , configureCheckProject
    , isReferenceReady
  )
where

import           Control.Applicative.Combinators          (skipManyTill, (<|>))
import           Control.Exception                        (catch, throwIO)
import           Control.Lens                             (_1, _Just,
                                                           traverseOf, (&),
                                                           (.~), (?~), (^.))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                               as A
import           Data.Bool                                (bool)
import           Data.Default
import           Data.List.Extra                          (find)
import           Data.Proxy
import qualified Data.Text                                as T
import           Development.IDE                          (GhcVersion (..),
                                                           ghcVersion)
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Test                        as Test
import           System.Directory
import           System.FilePath
import           System.Info.Extra                        (isMac, isWindows)
import qualified System.IO.Extra
import           System.IO.Temp
import           System.Time.Extra                        (Seconds, duration,
                                                           sleep)
import           Test.Tasty                               (TestTree)
import           Test.Tasty.ExpectedFailure               (expectFailBecause,
                                                           ignoreTestBecause)
import           Test.Tasty.HUnit                         (Assertion,
                                                           HasCallStack,
                                                           assertFailure)

import           Control.Arrow
import qualified Data.List                                as List
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.String.Interpolate                  (__i)
import qualified Data.Text.Internal.Search                as T
import qualified Data.Text.Utf16.Rope.Mixed               as Rope
import           Development.IDE.Plugin.Completions.Logic (getCompletionPrefixFromRope)
import           Development.IDE.Plugin.Completions.Types (PosPrefixInfo (..))
import           Development.IDE.Plugin.Test              (TestRequest (..),
                                                           WaitForIdeRuleResult (ideResultSuccess))
import           Development.IDE.Test.Diagnostic
import           GHC.TypeLits
import           Ide.Plugin.Config
import           Language.LSP.Test

noLiteralCaps :: ClientCapabilities
noLiteralCaps = def & L.textDocument ?~ textDocumentCaps
  where
    textDocumentCaps = def { _codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) Nothing Nothing Nothing Nothing Nothing Nothing

codeActionSupportCaps :: ClientCapabilities
codeActionSupportCaps = def & L.textDocument ?~ textDocumentCaps
  where
    textDocumentCaps = def { _codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) (Just literalSupport) (Just True) Nothing Nothing Nothing Nothing
    literalSupport = ClientCodeActionLiteralOptions (ClientCodeActionKindOptions [])

codeActionResolveCaps :: ClientCapabilities
codeActionResolveCaps = Test.fullLatestClientCaps
                          & (L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport . _Just) .~ ClientCodeActionResolveOptions {_properties= ["edit"]}
                          & (L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just) .~ True

codeActionNoResolveCaps :: ClientCapabilities
codeActionNoResolveCaps = Test.fullLatestClientCaps
                          & (L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport) .~ Nothing
                          & (L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just) .~ False

codeActionNoInlayHintsCaps :: ClientCapabilities
codeActionNoInlayHintsCaps = Test.fullLatestClientCaps
                          & (L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport) .~ Nothing
                          & (L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just) .~ False
                          & (L.textDocument . _Just . L.inlayHint) .~ Nothing
-- ---------------------------------------------------------------------
-- Environment specification for ignoring tests
-- ---------------------------------------------------------------------

data EnvSpec = HostOS OS | GhcVer GhcVersion
    deriving (Show, Eq)

matchesCurrentEnv :: EnvSpec -> Bool
matchesCurrentEnv (HostOS os)  = hostOS == os
matchesCurrentEnv (GhcVer ver) = ghcVersion == ver

data OS = Windows | MacOS | Linux
    deriving (Show, Eq)

hostOS :: OS
hostOS
    | isWindows = Windows
    | isMac = MacOS
    | otherwise = Linux

-- | Mark as broken if /any/ of the environmental specs matches the current environment.
knownBrokenInEnv :: [EnvSpec] -> String -> TestTree -> TestTree
knownBrokenInEnv envSpecs reason
    | any matchesCurrentEnv envSpecs = expectFailBecause reason
    | otherwise = id

-- | Mark as broken if /all/ environmental specs match the current environment.
knownBrokenInSpecificEnv :: [EnvSpec] -> String -> TestTree -> TestTree
knownBrokenInSpecificEnv envSpecs reason
    | all matchesCurrentEnv envSpecs = expectFailBecause reason
    | otherwise = id

knownBrokenOnWindows :: String -> TestTree -> TestTree
knownBrokenOnWindows = knownBrokenInEnv [HostOS Windows]

knownBrokenForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
knownBrokenForGhcVersions vers = knownBrokenInEnv (map GhcVer vers)

-- | IgnoreTest if /any/ of environmental spec mathces the current environment.
ignoreInEnv :: [EnvSpec] -> String -> TestTree -> TestTree
ignoreInEnv envSpecs reason
    | any matchesCurrentEnv envSpecs = ignoreTestBecause reason
    | otherwise = id

ignoreForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
ignoreForGhcVersions vers = ignoreInEnv (map GhcVer vers)

-- | Mark as broken if GHC does not match only work versions.
onlyWorkForGhcVersions :: (GhcVersion -> Bool) -> String -> TestTree -> TestTree
onlyWorkForGhcVersions p reason =
    if p ghcVersion
        then id
        else expectFailBecause reason

-- | Ignore the test if GHC does not match only work versions.
onlyRunForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
onlyRunForGhcVersions vers =
    if ghcVersion `elem` vers
    then const id
    else ignoreTestBecause

-- ---------------------------------------------------------------------

-- | Like 'withCurrentDirectory', but will copy the directory over to the system
-- temporary directory first to avoid haskell-language-server's source tree from
-- interfering with the cradle.
--
-- Ignores directories containing build artefacts to avoid interference and
-- provide reproducible test-behaviour.
withCurrentDirectoryInTmp :: FilePath -> IO a -> IO a
withCurrentDirectoryInTmp dir f =
  withTempCopy ignored dir $ \newDir ->
    withCurrentDirectory newDir f
  where
    ignored = ["dist", "dist-newstyle", ".stack-work"]


-- | Like 'withCurrentDirectory', but will copy the directory over to the system
-- temporary directory first to avoid haskell-language-server's source tree from
-- interfering with the cradle.
--
-- You may specify directories to ignore, but should be careful to maintain reproducibility.
withCurrentDirectoryInTmp' :: [FilePath] -> FilePath -> IO a -> IO a
withCurrentDirectoryInTmp' ignored dir f =
  withTempCopy ignored dir $ \newDir ->
    withCurrentDirectory newDir f

-- | Example call: @withTempCopy ignored src f@
--
-- Copy directory 'src' to into a temporary directory ignoring any directories
-- (and files) that are listed in 'ignored'. Pass the temporary directory
-- containing the copied sources to the continuation.
withTempCopy :: [FilePath] -> FilePath -> (FilePath -> IO a) -> IO a
withTempCopy ignored srcDir f = do
  withSystemTempDirectory "hls-test" $ \newDir -> do
    copyDir ignored srcDir newDir
    f newDir

-- | Example call: @copyDir ignored src dst@
--
-- Copy directory 'src' to 'dst' ignoring any directories (and files)
-- that are listed in 'ignored'.
copyDir :: [FilePath] -> FilePath -> FilePath -> IO ()
copyDir ignored src dst = do
  cnts <- listDirectory src
  forM_ cnts $ \file -> do
    unless (file `elem` ignored) $ do
      let srcFp = src </> file
          dstFp = dst </> file
      isDir <- doesDirectoryExist srcFp
      if isDir
        then createDirectory dstFp >> copyDir ignored srcFp dstFp
        else copyFile srcFp dstFp

fromAction :: (Command |? CodeAction) -> CodeAction
fromAction (InR action) = action
fromAction _            = error "Not a code action"

fromCommand :: (Command |? CodeAction) -> Command
fromCommand (InL command) = command
fromCommand _             = error "Not a command"

onMatch :: [a] -> (a -> Bool) -> String -> IO a
onMatch as predicate err = maybe (fail err) return (find predicate as)

noMatch :: [a] -> (a -> Bool) -> String -> IO ()
noMatch [] _ _           = pure ()
noMatch as predicate err = bool (pure ()) (fail err) (any predicate as)

inspectDiagnostic :: [Diagnostic] -> [T.Text] -> IO Diagnostic
inspectDiagnostic diags s = onMatch diags (\ca -> all (`T.isInfixOf` (ca ^. L.message)) s) err
    where err = "expected diagnostic matching '" ++ show s ++ "' but did not find one"

inspectDiagnosticAny :: [Diagnostic] -> [T.Text] -> IO Diagnostic
inspectDiagnosticAny diags s = onMatch diags (\ca -> any (`T.isInfixOf` (ca ^. L.message)) s) err
    where err = "expected diagnostic matching one of'" ++ show s ++ "' but did not find one"

expectDiagnostic :: [Diagnostic] -> [T.Text] -> IO ()
expectDiagnostic diags s = void $ inspectDiagnostic diags s

inspectCodeAction :: [Command |? CodeAction] -> [T.Text] -> IO CodeAction
inspectCodeAction cars s = fromAction <$> onMatch cars predicate err
    where predicate (InR ca) = all (`T.isInfixOf` (ca ^. L.title)) s
          predicate _        = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"

expectCodeAction :: [Command |? CodeAction] -> [T.Text] -> IO ()
expectCodeAction cars s = void $ inspectCodeAction cars s

dontExpectCodeAction :: [Command |? CodeAction] -> [T.Text] -> IO ()
dontExpectCodeAction cars s =
  noMatch cars predicate err
    where predicate (InR ca) = all (`T.isInfixOf` (ca ^. L.title)) s
          predicate _        = False
          err = "didn't expected code action matching '" ++ show s ++ "' but found one anyway"


inspectCommand :: [Command |? CodeAction] -> [T.Text] -> IO Command
inspectCommand cars s = fromCommand <$> onMatch cars predicate err
    where predicate (InL command) = all  (`T.isInfixOf` (command ^. L.title)) s
          predicate _             = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"

waitForDiagnosticsFrom :: TextDocumentIdentifier -> Test.Session [Diagnostic]
waitForDiagnosticsFrom doc = do
    diagsNot <- skipManyTill Test.anyMessage (Test.message SMethod_TextDocumentPublishDiagnostics)
    let diags = diagsNot ^. L.params . L.diagnostics
    if doc ^. L.uri /= diagsNot ^. L.params . L.uri
       then waitForDiagnosticsFrom doc
       else return diags


waitForTypecheck :: TextDocumentIdentifier -> Test.Session Bool
waitForTypecheck tid = ideResultSuccess <$> waitForAction "typecheck" tid

waitForActionWithDiagnosticsFromDocs :: (HasCallStack) => [TextDocumentIdentifier] -> Test.Session [[Diagnostic]]
waitForActionWithDiagnosticsFromDocs docs = do
  -- The test request waits until HLS has no pending diagnostic work. While
  -- waiting for the response, lsp-test consumes publishDiagnostics messages and
  -- updates its current-diagnostics cache.
  mapM_ waitForTypecheck docs
  void $ callTestPluginWithDiag WaitForDiagnosticPublished
  mapM Test.getCurrentDiagnostics docs

flushMessages :: Test.Session ()
flushMessages = do
    -- let cm = SMethod_CustomMethod (Proxy @"non-existent-method")
    let cm = SMethod_CustomMethod (Proxy @"test")
    i <- Test.sendRequest cm A.Null
    void (Test.responseForId cm i) <|> ignoreOthers cm i
    where
        ignoreOthers cm i = skipManyTill Test.anyMessage (Test.responseForId cm i) >> flushMessages


waitForDiagnosticsFromSource :: TextDocumentIdentifier -> String -> Test.Session [Diagnostic]
waitForDiagnosticsFromSource doc src = do
      diags <- concat <$> waitForActionWithDiagnosticsFromDocs [doc]
      return $ filter (\d -> d ^. L.source == Just (T.pack src)) diags

expectDiagnosticsEmpty :: TextDocumentIdentifier -> String -> Test.Session ()
expectDiagnosticsEmpty doc src = do
    diagsA <- concat <$> waitForActionWithDiagnosticsFromDocs [doc]
    let diags = filter (\d -> d ^. L.source == Just (T.pack src)) diagsA
    unless (null diags) $
        liftIO $ assertFailure $ "Expected no diagnostics for " <> show (doc ^. L.uri) <>
            " got " <> show diags

-- | 'waitForExpectedDiagnosticsFromDocs' waits for diagnostics to be published for the given documents,
-- and checks that they match the expected diagnostics. The expected diagnostics are specified as a list
-- of pairs of 'TextDocumentIdentifier' and a list of 'ExpectedDiagnostic'.
-- timeout is outdated and should be removed, as the function will until the system is idle,
-- which is the correct behaviour for the tests.
expectNoMoreDiagnostics :: HasCallStack => Seconds -> Test.Session ()
expectNoMoreDiagnostics _timeout = do
    diags <- callTestPluginWithDiag WaitForDiagnosticPublished
    unless (null diags) $ liftIO $
      assertFailure $
        "Expected no diagnostics, but got: "
          <> show (map unwrapDiagnostic diags)

expectDiagnosticsWithTags :: HasCallStack => [(FilePath, [ExpectedDiagnosticWithTag])] -> Test.Session ()
expectDiagnosticsWithTags expected = do
    let toSessionPath = getDocUri >=> liftIO . canonicalizeUri >=> pure . toNormalizedUri
        next = unwrapDiagnostic <$> skipManyTill anyMessage diagnostic
    expected' <- Map.fromListWith (<>) <$> traverseOf (traverse . _1) toSessionPath expected
    expectDiagnosticsWithTags' next expected'


expectDiagnosticsWithTags' ::
  HasCallStack =>
  Session (Uri, [Diagnostic]) ->
  Map.Map NormalizedUri [ExpectedDiagnosticWithTag] ->
  Session ()
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

requireDiagnosticM
    :: (Foldable f, Show (f Diagnostic), HasCallStack)
    => f Diagnostic
    -> ExpectedDiagnosticWithTag
    -> Assertion
requireDiagnosticM actuals expected = case requireDiagnostic actuals expected of
    Nothing  -> pure ()
    Just err -> assertFailure err

expectedDiagnosticWithNothing :: ExpectedDiagnostic -> ExpectedDiagnosticWithTag
expectedDiagnosticWithNothing (ds, c, t, code) = (ds, c, t, code, Nothing)


expectMessages :: SMethod m -> Seconds -> (TServerMessage m -> Session ()) -> Session ()
expectMessages m timeout handle = do
    -- Give any further diagnostic messages time to arrive.
    i <- callTestPluginWithSMethod m WaitForDiagnosticPublished
    -- Send a dummy message to provoke a response from the server.
    -- This guarantees that we have at least one message to
    -- process, so message won't block or timeout.
    -- let cm = SMethod_CustomMethod (Proxy @"test")
    -- i <- sendRequest cm $ A.toJSON GetShakeSessionQueueCount
    mapM_ handle i

-- | It is not possible to use 'expectDiagnostics []' to assert the absence of diagnostics,
--   only that existing diagnostics have been cleared.
--
--   Rather than trying to assert the absence of diagnostics, introduce an
--   expected diagnostic (e.g. a redundant import) and assert the singleton diagnostic.
expectDiagnostics :: HasCallStack => [(FilePath, [ExpectedDiagnostic])] -> Session ()
expectDiagnostics
  = expectDiagnosticsWithTags
  . map (second (map expectedDiagnosticWithNothing))


filePathTextDocumentIdentifier :: FilePath -> Session TextDocumentIdentifier
filePathTextDocumentIdentifier fp =
  TextDocumentIdentifier <$> getDocUri fp

waitForExpectedDiagnosticsFromFilePath :: (HasCallStack) => [(FilePath, [ExpectedDiagnostic])] -> Session ()
waitForExpectedDiagnosticsFromFilePath oxs = do
    -- merge diagnostics for same file paths
  let xs = Map.toList $ Map.fromListWith (++) [(fp, ed) | (fp, ed) <- oxs]
  res <- forM xs $ \(fp, ed) -> do
    tdi <- filePathTextDocumentIdentifier fp
    return (tdi, ed)
  waitForExpectedDiagnosticsFromDocs res

waitForExpectedDiagnosticsFromDocsOne :: (HasCallStack) => (TextDocumentIdentifier, [ExpectedDiagnostic]) -> Session ()
waitForExpectedDiagnosticsFromDocsOne x = waitForExpectedDiagnosticsFromDocs [x]

waitForExpectedDiagnosticsFromDocs :: (HasCallStack) => [(TextDocumentIdentifier, [ExpectedDiagnostic])] -> Session ()
waitForExpectedDiagnosticsFromDocs expected = do
  docDiags <- waitForActionWithDiagnosticsFromDocs (map fst expected)
  forM_ (zip expected docDiags) $ \((doc, exDiags), diags) -> do
    checkDiagnosticsForDoc doc exDiags diags

expectCurrentDiagnostics :: HasCallStack => TextDocumentIdentifier -> [ExpectedDiagnostic] -> Session ()
expectCurrentDiagnostics doc expected = do
    diags <- getCurrentDiagnostics doc
    checkDiagnosticsForDoc doc expected diags

checkDiagnosticsForDoc :: HasCallStack => TextDocumentIdentifier -> [ExpectedDiagnostic] -> [Diagnostic] -> Session ()
checkDiagnosticsForDoc TextDocumentIdentifier {_uri} expected obtained = do
    let expected' = Map.singleton nuri (map expectedDiagnosticWithNothing expected)
        nuri = toNormalizedUri _uri
    expectDiagnosticsWithTags' (return (_uri, obtained)) expected'


waitForCustomMessage :: T.Text -> (A.Value -> Maybe res) -> Session res
waitForCustomMessage msg pred =
    skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess (SMethod_CustomMethod p) (NotMess TNotificationMessage{_params = value})
            | symbolVal p == T.unpack msg -> pred value
        _ -> Nothing

waitForGC :: Session [T.Text]
waitForGC = waitForCustomMessage "ghcide/GC" $ \v ->
    case A.fromJSON v of
        A.Success x -> Just x
        _           -> Nothing

configureCheckProject :: Bool -> Session ()
configureCheckProject overrideCheckProject = setConfigSection "haskell" (A.toJSON $ def{checkProject = overrideCheckProject})

-- | Pattern match a message from ghcide indicating that a file has been indexed
isReferenceReady :: FilePath -> Session ()
isReferenceReady p = void $ referenceReady (equalFilePath p)

referenceReady :: (FilePath -> Bool) -> Session FilePath
referenceReady pred = satisfyMaybe $ \case
  FromServerMess (SMethod_CustomMethod p) (NotMess TNotificationMessage{_params})
    | A.Success fp <- A.fromJSON _params
    , pred fp
    , symbolVal p == "ghcide/reference/ready"
    -> Just fp
  _ -> Nothing

diagnostic :: Session (TNotificationMessage Method_TextDocumentPublishDiagnostics)
diagnostic = Test.message SMethod_TextDocumentPublishDiagnostics

canonicalizeUri :: Uri -> IO Uri
canonicalizeUri uri = filePathToUri <$> canonicalizePath (fromJust (uriToFilePath uri))

unwrapDiagnostic :: TServerMessage Method_TextDocumentPublishDiagnostics  -> (Uri, [Diagnostic])
unwrapDiagnostic diagsNot = (diagsNot^. L.params . L.uri, diagsNot^. L.params . L.diagnostics)

callTestPluginWithSMethod ::
    forall {t :: MessageKind} {a} {m :: Method ServerToClient t}.
    A.ToJSON a => SServerMethod m -> a -> Test.Session [TMessage m]
callTestPluginWithSMethod sm cmd = do
    let cm = SMethod_CustomMethod (Proxy @"test")
    waitId <- Test.sendRequest cm (A.toJSON cmd)
    let go acc = do
            res <- skipManyTill Test.anyMessage $ do

                (fmap Right (Test.responseForId cm waitId) <|> fmap Left (Test.message sm))
            case res of
                            Right TResponseMessage{_result} -> return (_result, acc)
                            Left a -> go (a:acc)
    (res, diagsNots) <- go []
    case res of
        Left (TResponseError t err _) -> error $ show t <> ": " <> T.unpack err
        Right _a                      -> return diagsNots

-- callTestPlugin :: (A.FromJSON b) => TestRequest -> Test.Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) b)
-- callTestPlugin cmd = do
--     let cm = SMethod_CustomMethod (Proxy @"test")
--     waitId <- Test.sendRequest cm (A.toJSON cmd)
--     TResponseMessage{_result} <- skipManyTill Test.anyMessage $ Test.responseForId cm waitId
--     return $ do
--       e <- _result
--       case A.fromJSON e of
--         A.Error err -> Left $ TResponseError (InR ErrorCodes_InternalError) (T.pack err) Nothing
--         A.Success a -> pure a

tryCallTestPlugin :: (A.FromJSON b) => TestRequest -> Test.Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) b)
tryCallTestPlugin cmd = do
    let cm = SMethod_CustomMethod (Proxy @"test")
    waitId <- Test.sendRequest cm (A.toJSON cmd)
    TResponseMessage{_result} <- skipManyTill Test.anyMessage $ Test.responseForId cm waitId
    return $ case _result of
         Left e -> Left e
         Right json -> case A.fromJSON json of
             A.Success a -> Right a
             A.Error e   -> error e

callTestPlugin :: (A.FromJSON b) => TestRequest -> Test.Session b
callTestPlugin cmd = do
    res <- tryCallTestPlugin cmd
    case res of
        Left (TResponseError t err _) -> error $ show t <> ": " <> T.unpack err
        Right a                       -> pure a

callTestPluginWithDiag ::
    A.ToJSON a => a -> Test.Session [TNotificationMessage Method_TextDocumentPublishDiagnostics]
callTestPluginWithDiag = callTestPluginWithSMethod SMethod_TextDocumentPublishDiagnostics


failIfSessionTimeout :: HasCallStack => IO a -> IO a
failIfSessionTimeout action = action `catch` errorHandler
    where errorHandler :: Test.SessionException -> IO a
          errorHandler e@(Test.Timeout _) = assertFailure $ show e
          errorHandler e                  = throwIO e

-- ---------------------------------------------------------------------
getCompletionByLabel :: MonadIO m => T.Text -> [CompletionItem] -> m CompletionItem
getCompletionByLabel desiredLabel compls =
    case find (\c -> c ^. L.label == desiredLabel) compls of
        Just c -> pure c
        Nothing -> liftIO . assertFailure $
            "Completion with label " <> show desiredLabel
            <> " not found in " <> show (fmap (^. L.label) compls)

-- ---------------------------------------------------------------------
-- Run with a canonicalized temp dir
withCanonicalTempDir :: (FilePath -> IO a) -> IO a
withCanonicalTempDir f = System.IO.Extra.withTempDir $ \dir -> do
  dir' <- canonicalizePath dir
  f dir'

-- ----------------------------------------------------------------------------
-- Extract Position data from the source file itself.
-- ----------------------------------------------------------------------------

-- | Pretty labelling for tests that use the parameterised test helpers.
mkParameterisedLabel :: PosPrefixInfo -> String
mkParameterisedLabel posPrefixInfo = unlines
    [ "Full Line:       \"" <> T.unpack (fullLine posPrefixInfo) <> "\""
    , "Cursor Column:   \"" <> replicate (fromIntegral $ cursorPos posPrefixInfo ^. L.character) ' ' ++ "^" <> "\""
    , "Prefix Text:     \"" <> T.unpack (prefixText posPrefixInfo) <> "\""
    ]

-- | Given a in-memory representation of a file, where a user can specify the
-- current cursor position using a '^' in the next line.
--
-- This function allows to generate multiple tests for a single input file, without
-- the hassle of calculating by hand where there cursor is supposed to be.
--
-- Example (line number has been added for readability):
--
-- @
--   0: foo = 2
--   1:  ^
--   2: bar =
--   3:      ^
-- @
--
-- This example input file contains two cursor positions (y, x), at
--
-- * (1, 1), and
-- * (3, 5).
--
-- 'extractCursorPositions' will search for '^' characters, and determine there are
-- two cursor positions in the text.
-- First, it will normalise the text to:
--
-- @
--   0: foo = 2
--   1: bar =
-- @
--
-- stripping away the '^' characters. Then, the actual cursor positions are:
--
-- * (0, 1) and
-- * (2, 5).
--
extractCursorPositions :: T.Text -> (T.Text, [PosPrefixInfo])
extractCursorPositions t =
    let
        textLines = T.lines t
        foldState = List.foldl' go emptyFoldState textLines
        finalText = foldStateToText foldState
        reconstructCompletionPrefix pos = getCompletionPrefixFromRope pos (Rope.fromText finalText)
        cursorPositions = reverse . fmap reconstructCompletionPrefix $ foldStatePositions foldState
    in
        (finalText, cursorPositions)

    where
        go foldState l = case T.indices "^" l of
            [] -> addTextLine foldState l
            xs -> List.foldl' addTextCursor foldState xs

-- | 'FoldState' is an implementation detail used to parse some file contents,
-- extracting the cursor positions identified by '^' and producing a cleaned
-- representation of the file contents.
data FoldState = FoldState
    { foldStateRows      :: !Int
    -- ^ The row index of the cleaned file contents.
    --
    -- For example, the file contents
    --
    -- @
    --   0: foo
    --   1: ^
    --   2: bar
    -- @
    -- will report that 'bar' is actually occurring in line '1', as '^' is
    -- a cursor position.
    -- Lines containing cursor positions are removed.
    , foldStatePositions :: ![Position]
    -- ^ List of cursors positions found in the file contents.
    --
    -- List is stored in reverse for efficient 'cons'ing
    , foldStateFinalText :: ![T.Text]
    -- ^ Final file contents with all lines containing cursor positions removed.
    --
    -- List is stored in reverse for efficient 'cons'ing
    }

emptyFoldState :: FoldState
emptyFoldState = FoldState
    { foldStateRows = 0
    , foldStatePositions = []
    , foldStateFinalText = []
    }

-- | Produce the final file contents, without any lines containing cursor positions.
foldStateToText :: FoldState -> T.Text
foldStateToText state = T.unlines $ reverse $ foldStateFinalText state

-- | We found a '^' at some location! Add it to the list of known cursor positions.
--
-- If the row index is '0', we throw an error, as there can't be a cursor position above the first line.
addTextCursor :: FoldState -> Int -> FoldState
addTextCursor state col
    | foldStateRows state <= 0 = error $ "addTextCursor: Invalid '^' found at: " <> show (col, foldStateRows state)
    | otherwise = state
        { foldStatePositions = Position (fromIntegral (foldStateRows state) - 1) (fromIntegral col) : foldStatePositions state
        }

addTextLine :: FoldState -> T.Text -> FoldState
addTextLine state l = state
    { foldStateFinalText = l : foldStateFinalText state
    , foldStateRows = foldStateRows state + 1
    }

waitForAction :: String -> TextDocumentIdentifier -> Test.Session WaitForIdeRuleResult
waitForAction key TextDocumentIdentifier{_uri} =
    callTestPlugin (WaitForIdeRule key _uri)

getInterfaceFilesDir :: TextDocumentIdentifier -> Test.Session FilePath
getInterfaceFilesDir TextDocumentIdentifier{_uri} = callTestPlugin (GetInterfaceFilesDir _uri)

-- garbageCollectDirtyKeys :: CheckParents -> Int -> Session [String]
-- garbageCollectDirtyKeys parents age = callTestPlugin (GarbageCollectDirtyKeys parents age)

getStoredKeys :: Test.Session [T.Text]
getStoredKeys = callTestPlugin GetStoredKeys

-- | Wait for the build queue to be empty
waitForBuildQueue :: Test.Session Seconds
waitForBuildQueue = do
    let m = SMethod_CustomMethod (Proxy @"test")
    waitId <- Test.sendRequest m (A.toJSON WaitForShakeQueue)
    (td, resp) <- duration $ skipManyTill Test.anyMessage $ Test.responseForId m waitId
    case resp of
        TResponseMessage{_result=Right A.Null} -> return td
        -- assume a ghcide binary lacking the WaitForShakeQueue method
        _                                      -> return 0

getFilesOfInterest :: Test.Session [FilePath]
getFilesOfInterest = callTestPlugin GetFilesOfInterest
