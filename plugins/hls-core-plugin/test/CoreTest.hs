{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified CompletionTests
import qualified CradleTests
import qualified FindDefinitionAndHoverTests
import qualified HighlightTests
import qualified InitializeResponseTests
import qualified OutlineTests
import qualified ReferenceTests
import           Test.Hls                    (defaultTestRunner, testGroup)


main :: IO ()
main =
  defaultTestRunner $
    testGroup
      "core"
      [
        InitializeResponseTests.tests
      , OutlineTests.tests
      , CompletionTests.tests
      , HighlightTests.tests
      , FindDefinitionAndHoverTests.tests
      , ReferenceTests.tests
      , CradleTests.tests
      ]
