{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Ide.Plugin.Core     as Core
import           System.FilePath     ((</>))
import           Test.Hls            (PluginTestDescriptor,
                                      mkPluginTestDescriptor)
import qualified Test.Hls.FileSystem as FS
import           Test.Hls.FileSystem (file, text)

corePlugin :: PluginTestDescriptor Core.CoreLog
corePlugin = mkPluginTestDescriptor Core.descriptor "core"

directFile :: FilePath -> Text -> [FS.FileTree]
directFile fp content =
  [ FS.directCradle [Text.pack fp]
  , file fp (text content)
  ]

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

testDataDir :: FilePath
testDataDir = "plugins" </> "core-plugin" </> "test" </> "testdata"
