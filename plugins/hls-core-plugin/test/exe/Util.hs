{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Default        (Default (..))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Development.IDE     (GhcVersion, ghcVersion)
import qualified Ide.Plugin.Core     as Core
import           Language.LSP.Test   (Session)
import           System.FilePath     ((</>))
import           System.Info.Extra
import           Test.Hls            (PluginTestDescriptor, TestName, TestTree,
                                      expectFailBecause, ignoreTestBecause,
                                      mkPluginTestDescriptor,
                                      runSessionWithServerInTmpDir, testCase)
import qualified Test.Hls.FileSystem as FS
import           Test.Hls.FileSystem (file, text)


runSessionWithCorePlugin :: FS.VirtualFileTree -> Session a -> IO a
runSessionWithCorePlugin = runSessionWithServerInTmpDir def corePlugin

runSessionWithCorePluginEmpty :: [Text] -> Session a -> IO a
runSessionWithCorePluginEmpty fps = runSessionWithCorePlugin (mkFs [FS.directCradle fps])

runSessionWithCorePluginSingleFile :: FilePath -> Text -> Session a -> IO a
runSessionWithCorePluginSingleFile fp content = runSessionWithCorePlugin (mkSingleFileFs fp content)

testSessionWithCorePluginSingleFile :: TestName -> FilePath -> Text -> Session () -> TestTree
testSessionWithCorePluginSingleFile caseName fp content = testCase caseName . runSessionWithCorePluginSingleFile fp content

corePlugin :: PluginTestDescriptor Core.CoreLog
corePlugin = mkPluginTestDescriptor Core.descriptor "core"

mkSingleFileFs :: FilePath -> Text -> FS.VirtualFileTree
mkSingleFileFs fp = mkFs . directFile fp

directFile :: FilePath -> Text -> [FS.FileTree]
directFile fp content =
  [ FS.directCradle [Text.pack fp]
  , file fp (text content)
  ]

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

testDataDir :: FilePath
testDataDir = "plugins" </> "core-plugin" </> "test" </> "testdata"


data BrokenOS = Linux | MacOS | Windows deriving (Show)

data IssueSolution = Broken | Ignore deriving (Show)

data BrokenTarget =
    BrokenSpecific BrokenOS [GhcVersion]
    -- ^Broken for `BrokenOS` with `GhcVersion`
    | BrokenForOS BrokenOS
    -- ^Broken for `BrokenOS`
    | BrokenForGHC [GhcVersion]
    -- ^Broken for `GhcVersion`
    deriving (Show)

ignoreFor :: BrokenTarget -> String -> TestTree -> TestTree
ignoreFor = knownIssueFor Ignore

-- | Known broken for specific os and ghc with reason.
knownBrokenFor :: BrokenTarget -> String -> TestTree -> TestTree
knownBrokenFor = knownIssueFor Broken

-- | Deal with `IssueSolution` for specific OS and GHC.
knownIssueFor :: IssueSolution -> BrokenTarget -> String -> TestTree -> TestTree
knownIssueFor solution = go . \case
    BrokenSpecific bos vers -> isTargetOS bos && isTargetGhc vers
    BrokenForOS bos         -> isTargetOS bos
    BrokenForGHC vers       -> isTargetGhc vers
    where
        isTargetOS = \case
            Windows -> isWindows
            MacOS   -> isMac
            Linux   -> not isWindows && not isMac

        isTargetGhc = elem ghcVersion

        go True = case solution of
            Broken -> expectFailBecause
            Ignore -> ignoreTestBecause
        go False = const id
