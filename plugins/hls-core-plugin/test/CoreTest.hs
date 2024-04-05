{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified InitializeResponseTests
import qualified OutlineTests
import           Test.Hls                (defaultTestRunner, testGroup)


main :: IO ()
main =
  defaultTestRunner $
    testGroup
      "core"
      [ InitializeResponseTests.tests
      , OutlineTests.tests
      ]
