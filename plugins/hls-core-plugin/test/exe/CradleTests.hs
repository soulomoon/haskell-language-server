{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CradleTests (tests) where

import           Control.Applicative.Combinators
import           Control.Monad.IO.Class          (liftIO)
import           Data.Row
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat      (GhcVersion (..))
import           Development.IDE.GHC.Util
-- import           Development.IDE.Test            (expectDiagnostics,
--                                                   expectDiagnosticsWithTags,
--                                                   expectNoMoreDiagnostics,
--                                                   isReferenceReady,
--                                                   waitForAction)
import           Development.IDE.Types.Location
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.FilePath
import           System.IO.Extra                 hiding (withTempDir)
-- import Test.QuickCheck.Instances ()
import           Control.Lens                    ((^.))
import           Development.IDE.Plugin.Test     (WaitForIdeRuleResult (..))
import           GHC.TypeLits                    (symbolVal)
import           System.Directory                (getCurrentDirectory)
import           Test.Hls                        (captureKickDiagnostics,
                                                  expectNoKickDiagnostic,
                                                  waitForAction,
                                                  waitForAllProgressDone)
import qualified Test.Hls.FileSystem             as FS
import           Test.Hls.FileSystem             (file, text, toAbsFp)
import           Test.Hls.Util                   (knownBrokenForGhcVersions)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Util                            (checkDefs, expectDiagnostics,
                                                  expectDiagnosticsWithTags,
                                                  isReferenceReady, mkFs, mkL,
                                                  runSessionWithCorePluginNoVsf,
                                                  runSessionWithServerCorePlugin,
                                                  testSessionWithCorePlugin,
                                                  testSessionWithCorePluginEmptyVsf,
                                                  testSessionWithCorePluginSubDir)


tests :: TestTree
tests = testGroup "cradle"
    [testGroup "dependencies" [sessionDepsArePickedUp]
    ,testGroup "ignore-fatal" [ignoreFatalWarning]
    ,testGroup "loading" [loadCradleOnlyonce, retryFailedCradle]
    ,testGroup "multi"   (multiTests "multi")
    ,knownBrokenForGhcVersions [GHC92] "multiple units not supported on 9.2"
       $ testGroup "multi-unit" (multiTests "multi-unit")
    ,testGroup "sub-directory"   [simpleSubDirectoryTest]
    ,knownBrokenForGhcVersions [GHC92] "multiple units not supported on 9.2"
      $ testGroup "multi-unit-rexport" [multiRexportTest]
    ]

loadCradleOnlyonce :: TestTree
loadCradleOnlyonce = testGroup "load cradle only once"
    [ testSessionWithCorePluginEmptyVsf "implicit" test
    , testSessionWithCorePlugin "direct" (mkFs [FS.directCradle ["B.hs", "A.hs"]]) test
    ]
    where
        test = do
            doc <- createDoc "B.hs" "haskell" "module B where\nimport Data.Foo"
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent . InR . (.==) #text $ "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0
            _ <- createDoc "A.hs" "haskell" "module A where\nimport LoadCradleBar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0

retryFailedCradle :: TestTree
retryFailedCradle = testSessionWithCorePluginEmptyVsf "retry failed" $ \fs -> do
  -- The false cradle always fails
  let hieContents = "cradle: {bios: {shell: \"false\"}}"
      hiePath = "hie.yaml"
  liftIO $ writeFile hiePath hieContents
  let aPath = "A.hs"
  doc <- createDoc aPath "haskell" "main = return ()"
  WaitForIdeRuleResult {..} <- handleEither (waitForAction "TypeCheck" doc)
  liftIO $ "Test assumption failed: cradle should error out" `assertBool` not ideResultSuccess

  -- Fix the cradle and typecheck again
  let validCradle = "cradle: {bios: {shell: \"echo A.hs\"}}"
  liftIO $ writeFileUTF8 hiePath $ T.unpack validCradle
  sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
         [FileEvent (filePathToUri $ toAbsFp fs "hie.yaml") FileChangeType_Changed ]

  WaitForIdeRuleResult {..} <- handleEither (waitForAction "TypeCheck" doc)
  liftIO $ "No joy after fixing the cradle" `assertBool` ideResultSuccess

handleEither :: Session (Either ResponseError b) -> Session b
handleEither sei = do
    ei <- sei
    case ei of
        Left e  -> liftIO $ assertFailure $ show e
        Right x -> pure x

cradleLoadedMessage :: Session FromServerMessage
cradleLoadedMessage = satisfy $ \case
        FromServerMess (SMethod_CustomMethod p) (NotMess _) -> symbolVal p == cradleLoadedMethod
        _                                            -> False

cradleLoadedMethod :: String
cradleLoadedMethod = "ghcide/cradle/loaded"

ignoreFatalWarning :: TestTree
ignoreFatalWarning = testSessionWithCorePluginSubDir "ignore-fatal-warning" "ignore-fatal" $ do
    _ <- openDoc "IgnoreFatal.hs" "haskell"
    diags <- captureKickDiagnostics
    liftIO $ assertBool "Expecting no warning" $ null diags


simpleSubDirectoryTest :: TestTree
simpleSubDirectoryTest =
  testSessionWithCorePluginSubDir "simple-subdirectory" "cabal-exe" $ do
    let mainPath = "a/src/Main.hs"
    _mdoc <- openDoc mainPath "haskell"
    waitForAllProgressDone
    expectDiagnosticsWithTags
      [("a/src/Main.hs", [(DiagnosticSeverity_Warning,(2,0), "Top-level binding", Nothing)]) -- So that we know P has been loaded
      ]

multiTests :: FilePath -> [TestTree]
multiTests dir =
  [simpleMultiTest dir, simpleMultiTest2 dir, simpleMultiTest3 dir, simpleMultiDefTest dir]

multiTestName :: FilePath -> String -> String
multiTestName dir name = "simple-" ++ dir ++ "-" ++ name

simpleMultiTest :: FilePath -> TestTree
simpleMultiTest variant = testSessionWithCorePluginSubDir (multiTestName variant "test") variant $ do
    let aPath = "a/A.hs"
        bPath = "b/B.hs"
    adoc <- openDoc aPath "haskell"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {..} <- handleEither $ waitForAction "TypeCheck" adoc
    liftIO $ assertBool "A should typecheck" ideResultSuccess
    WaitForIdeRuleResult {..} <- handleEither $ waitForAction "TypeCheck" bdoc
    liftIO $ assertBool "B should typecheck" ideResultSuccess
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    -- diags <- captureKickDiagnostics
    -- liftIO $ assertBool "Expecting no warning" $ null diags

-- Like simpleMultiTest but open the files in the other order
simpleMultiTest2 :: FilePath -> TestTree
simpleMultiTest2 variant = testSessionWithCorePluginSubDir (multiTestName variant "test2") variant $ \fs -> do
    let aPath = "a/A.hs"
        bPath = "b/B.hs"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {} <- handleEither $ waitForAction "TypeCheck" bdoc
    TextDocumentIdentifier auri <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady (toAbsFp fs aPath)
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    -- diags <- captureKickDiagnostics
    -- liftIO $ assertBool "Expecting no warning" $ null diags

-- Now with 3 components
simpleMultiTest3 :: FilePath -> TestTree
simpleMultiTest3 variant =
  testSessionWithCorePluginSubDir (multiTestName variant "test3") variant $ \fs -> do
    let aPath = "a/A.hs"
        bPath = "b/B.hs"
        cPath = "c/C.hs"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {} <- handleEither $ waitForAction "TypeCheck" bdoc
    TextDocumentIdentifier auri <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady (toAbsFp fs aPath)
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- handleEither $ waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 2 7)
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    -- diags <- captureKickDiagnostics
    -- liftIO $ assertBool "Expecting no warning" $ null diags


-- Like simpleMultiTest but open the files in component 'a' in a separate session
simpleMultiDefTest :: FilePath -> TestTree
simpleMultiDefTest variant = testSessionWithCorePluginSubDir (multiTestName variant "def-test") variant $ \fs -> do
    let aPath = "a/A.hs"
        bPath = "b/B.hs"
        aAbsPath = toAbsFp fs aPath
        rootAbs = toAbsFp fs ""
    adoc <- liftIO $ runSessionWithServerCorePlugin rootAbs $ do
      adoc <- openDoc aAbsPath "haskell"
    --   skipManyTill anyMessage $ isReferenceReady $ aAbsPath
    --   closeDoc adoc
      pure adoc
    bdoc <- openDoc bPath "haskell"
    -- locs <- getDefinitions bdoc (Position 2 7)
    -- let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    -- checkDefs locs (pure [fooL])
    -- diags <- captureKickDiagnostics
    -- liftIO $ assertBool "Expecting no warning" $ null diags
    return ()

multiRexportTest :: TestTree
multiRexportTest =
  testSessionWithCorePluginSubDir "multi-unit-reexport-test"  "multi-unit-reexport" $ do
    let cPath = "c/C.hs"
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- handleEither $ waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 3 7)
    let aPath = "a/A.hs"
    let fooL = mkL (filePathToUri aPath) 2 0 2 3
    checkDefs locs (pure [fooL])
    -- diags <- captureKickDiagnostics
    -- liftIO $ assertBool "Expecting no warning" $ null diags

sessionDepsArePickedUp :: TestTree
sessionDepsArePickedUp = testSessionWithCorePlugin
  "session-deps-are-picked-up" (mkFs [file "Foo.hs" (text fooContent) , file "hie.yaml" (text "cradle: {direct: {arguments: [-XOverloadedStrings]}}")])
  $ \fs -> do
    doc <- openDoc "Foo.hs" "haskell"
    expectNoKickDiagnostic
    cwd <- liftIO getCurrentDirectory
    liftIO $
      writeFileUTF8
        "hie.yaml"
        "cradle: {direct: {arguments: []}}"
    liftIO $ (filePathToUri $ cwd </> "hie.yaml") @?= (filePathToUri $ toAbsFp fs "hie.yaml")
    sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams [FileEvent (filePathToUri $ cwd </> "hie.yaml") FileChangeType_Changed]
    -- Send change event.
    let change =
          TextDocumentContentChangeEvent $ InL $ #range .== Range (Position 4 0) (Position 4 0)
                                              .+ #rangeLength .== Nothing
                                              .+ #text .== "\n"
    changeDoc doc [change]
    expectDiagnostics [("Foo.hs", [(DiagnosticSeverity_Error, (3, 6), "Couldn't match type")])]
    return ()

  where
    fooContent2 =
      unlines
        [ "module Foo where",
          "import Data.Text",
          "foo :: Text",
          "",
          "foo = \"hello\"",
          "x=1"
        ]

    fooContent =
      T.unlines
        [ "module Foo where",
          "import Data.Text",
          "foo :: Text",
          "foo = \"hello\""
        ]
