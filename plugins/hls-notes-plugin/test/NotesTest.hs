module Main (main) where

import           Data.List           (sort)
import           Ide.Plugin.Notes    (Log, descriptor)
import           System.FilePath     ((</>))
import           Test.Hls
import           Test.Hls.FileSystem (VirtualFileTree (..), copyDir)

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "notes"

main :: IO ()
main = defaultTestRunner $
  testGroup "Notes"
    [ gotoNoteTests
    , noteReferenceTests
    ]

runSessionWithServerNote :: FilePath -> (FilePath -> Session a) -> IO a
runSessionWithServerNote fp act =
    runSessionWithTestConfig def
        { testLspConfig = def
        , testPluginDescriptor = plugin
        , testDirLocation = VirtualFileTree [copyDir "./"] fp
        } act

noteReferenceTests :: TestTree
noteReferenceTests = testGroup "Note References"
   [
      testCase "multi_file" $ runSessionWithServerNote testDataDir $ \dir -> do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        refs <- getReferences doc (Position 21 15) False
        let fp = dir </> "NoteDef.hs"
        liftIO $ sort refs @?= sort [
            Location (filePathToUri (dir </> "Other.hs")) (Range (Position 6 13) (Position 6 13)),
            Location (filePathToUri fp) (Range (Position 9 9) (Position 9 9)),
            Location (filePathToUri fp) (Range (Position 5 67) (Position 5 67))
          ]
    ]

gotoNoteTests :: TestTree
gotoNoteTests = testGroup "Goto Note Definition"
    [
      testCase "single_file" $ runSessionWithServerNote testDataDir $ \dir -> do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 3 41)
        let fp = dir </> "NoteDef.hs"
        liftIO $ defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 11 9) (Position 11 9))]))
    , testCase "liberal_format" $ runSessionWithServerNote testDataDir $ \dir -> do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 5 64)
        let fp = dir </> "NoteDef.hs"
        liftIO $ defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 21 11) (Position 21 11))]))

    , testCase "invalid_note" $ runSessionWithServerNote testDataDir $ const $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 6 54)
        liftIO $ defs @?= InL (Definition (InR []))

    , testCase "no_note" $ runSessionWithServerNote testDataDir $ const $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 1 0)
        liftIO $ defs @?= InL (Definition (InR []))

    , testCase "unopened_file" $ runSessionWithServerNote testDataDir $ \dir -> do
        doc <- openDoc "Other.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 5 20)
        let fp = dir </> "NoteDef.hs"
        liftIO $ defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 15 6) (Position 15 6))]))
    ]

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-notes-plugin" </> "test" </> "testdata"
