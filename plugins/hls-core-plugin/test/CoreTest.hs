{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified InitializeResponseTests
import           Test.Hls                (defaultTestRunner, testGroup)


main :: IO ()
main =
  defaultTestRunner $
    testGroup
      "core"
      [ InitializeResponseTests.tests ]
