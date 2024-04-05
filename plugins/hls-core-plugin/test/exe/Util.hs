{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Default        (Default (..))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Ide.Plugin.Core     as Core
import           Language.LSP.Test   (Session)
import           System.FilePath     ((</>))
import           Test.Hls            (Assertion, PluginTestDescriptor, TestName,
                                      TestTree, mkPluginTestDescriptor,
                                      runSessionWithServerInTmpDir, testCase)
import qualified Test.Hls.FileSystem as FS
import           Test.Hls.FileSystem (file, text)


runSessionWithCorePlugin :: FS.VirtualFileTree -> Session a -> IO a
runSessionWithCorePlugin = runSessionWithServerInTmpDir def corePlugin

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
