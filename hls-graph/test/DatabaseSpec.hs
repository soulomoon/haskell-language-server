{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where

import           ActionSpec                              (itInThread)
import           Control.Exception                       (SomeException, throw)
import           Development.IDE.Graph                   (ShakeOptions, newKey,
                                                          shakeOptions)
import           Development.IDE.Graph.Database          (shakeNewDatabase,
                                                          shakeRunDatabase)
import           Development.IDE.Graph.Internal.Action   (apply1)
import           Development.IDE.Graph.Internal.Database (compute, incDatabase)
import           Development.IDE.Graph.Internal.Rules    (addRule)
import           Development.IDE.Graph.Internal.Types
import           Example
import           System.Time.Extra                       (timeout)
import           Test.Hspec


exractException :: [Either SomeException ()] -> Maybe StackException
exractException [] = Nothing
exractException (Left e : _) | Just ne@StackException{} <- fromGraphException e = return ne
exractException (_: xs) = exractException xs

shakeNewDatabaseWithLogger :: DBQue -> ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabaseWithLogger = shakeNewDatabase (const $ return ())

spec :: Spec
spec = do
    describe "Evaluation" $ do
        itInThread "detects cycles" $ \q -> do
            db <- shakeNewDatabaseWithLogger q shakeOptions $ do
                ruleBool
                addRule $ \Rule _old _mode -> do
                    True <- apply1 (Rule @Bool)
                    return $ RunResult ChangedRecomputeDiff "" () (return ())
            res <- timeout 1 $ shakeRunDatabase db $ pure $ apply1 (Rule @())
            let x = exractException =<< res
            let throwStack x = case x
                    of Just e  -> throw e
                       Nothing -> error "Expected a StackException, got none"
            throwStack x `shouldThrow` \StackException{} -> True

    describe "compute" $ do
      itInThread "build step and changed step updated correctly" $ \q -> do
        (ShakeDatabase _ _ theDb) <- shakeNewDatabaseWithLogger q shakeOptions $ do
          ruleStep
        let k = newKey $ Rule @()
        -- ChangedRecomputeSame
        r1@Result{resultChanged=rc1, resultBuilt=rb1} <- compute theDb emptyStack k RunDependenciesChanged Nothing
        incDatabase theDb Nothing
        -- ChangedRecomputeSame
        r2@Result{resultChanged=rc2, resultBuilt=rb2} <- compute theDb emptyStack k RunDependenciesChanged (Just r1)
        incDatabase theDb Nothing
        -- changed Nothing
        Result{resultChanged=rc3, resultBuilt=rb3} <- compute theDb emptyStack k RunDependenciesSame (Just r2)
        rc1 `shouldBe` Step 0
        rc2 `shouldBe` Step 0
        rc3 `shouldBe` Step 0

        rb1 `shouldBe` Step 0
        rb2 `shouldBe` Step 1
        rb3 `shouldBe` Step 1
