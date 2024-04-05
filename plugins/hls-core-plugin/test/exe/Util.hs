{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}

module Util where

import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Default                (Default (..))
import           Data.Foldable               (traverse_)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text                   as Text
import           Development.IDE             (GhcVersion, ghcVersion)
import qualified Ide.Plugin.Core             as Core
import           Language.LSP.Protocol.Types (Definition (..),
                                              DefinitionLink (..),
                                              Location (..), LocationLink (..),
                                              Null (..), Position (..),
                                              Range (..), UInt, Uri (..),
                                              filePathToUri, mkRange,
                                              type (|?) (InL, InR),
                                              uriToFilePath)
import           Language.LSP.Test           (Session)
import           System.Directory.Extra      (canonicalizePath)
import           System.FilePath             ((</>))
import           System.Info.Extra
import           Test.Hls                    (PluginTestDescriptor, TestName,
                                              TestTree, assertBool,
                                              expectFailBecause,
                                              ignoreTestBecause,
                                              mkPluginTestDescriptor,
                                              runSessionWithServerInTmpDir,
                                              testCase)
import qualified Test.Hls.FileSystem         as FS
import           Test.Hls.FileSystem         (copy, file, text)
import           Test.Tasty.HUnit            (Assertion, assertFailure, (@=?),
                                              (@?=))


pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

testSessionWithCorePlugin :: TestName -> FS.VirtualFileTree -> Session () -> TestTree
testSessionWithCorePlugin caseName vfs = testCase caseName . runSessionWithCorePlugin vfs

runSessionWithCorePlugin :: FS.VirtualFileTree -> Session a -> IO a
runSessionWithCorePlugin = runSessionWithServerInTmpDir def corePlugin

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

