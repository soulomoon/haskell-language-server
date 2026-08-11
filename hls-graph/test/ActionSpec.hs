{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module ActionSpec where

import           Control.Concurrent                      (MVar, newEmptyMVar,
                                                          readMVar)
import qualified Control.Concurrent                      as C
import           Control.Concurrent.Async                (AsyncCancelled,
                                                          cancel, waitCatch,
                                                          withAsync)
import           Control.Concurrent.STM
import           Control.Exception                       (finally,
                                                          fromException)
import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Maybe                              (isJust, isNothing)
import           Development.IDE.Graph                   (RuleResult,
                                                          shakeOptions)
import           Development.IDE.Graph.Classes           (Hashable, NFData)
import           Development.IDE.Graph.Database          (shakeNewDatabase,
                                                          shakeRunDatabase,
                                                          shakeRunDatabaseForKeys)
import           Development.IDE.Graph.Internal.Database (build, incDatabase)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Rule
import           Example
import           GHC.Generics                            (Generic)
import qualified StmContainers.Map                       as STM
import           System.IO.Unsafe                        (unsafePerformIO)
import           System.Timeout                          (timeout)
import           Test.Hspec


-- Park traversal after the preceding key has been handled. The test releases
-- this key normally; cancellation never crosses this unsafe test-only barrier.
{-# NOINLINE blockSecondKey #-}
blockSecondKey :: MVar () -> MVar () -> a -> a
blockSecondKey reached release value = unsafePerformIO $ do
  C.putMVar reached ()
  C.takeMVar release
  pure value

data ScopeRule = ScopeParent | ScopeLeft | ScopeRight
  deriving (Eq, Show, Generic, Hashable, NFData)

type instance RuleResult ScopeRule = ()

scopeRules :: Rules ()
scopeRules = addRule $ \key _old _mode -> do
  case key of
    ScopeParent -> void $ apply [ScopeLeft, ScopeRight]
    _           -> pure ()
  pure $ RunResult ChangedRecomputeDiff "" () (pure ())


spec :: Spec
spec = do
  describe "apply1" $ it "Test build update, Buggy dirty mechanism in hls-graph #4237" $ do
    let ruleStep1 :: MVar Int -> Rules ()
        ruleStep1 m = addRule $ \CountRule _old mode -> do
            -- depends on ruleSubBranch, it always changed if dirty
            _ :: Int <- apply1 SubBranchRule
            let r = 1
            case mode of
                -- it update the built step
                RunDependenciesChanged -> do
                    _ <- liftIO $ C.modifyMVar m $ \x -> return (x+1, x)
                    return $ RunResult ChangedRecomputeSame "" r (return ())
                -- this won't update the built step
                RunDependenciesSame ->
                    return $ RunResult ChangedNothing "" r (return ())
    count <- C.newMVar 0
    count1 <- C.newMVar 0
    db <- shakeNewDatabase shakeOptions $ do
      ruleSubBranch count
      ruleStep1 count1
    -- bootstrapping the database
    _ <- shakeRunDatabase db $ pure $ apply1 CountRule -- count = 1
    let child = newKey SubBranchRule
    let parent = newKey CountRule
    -- instruct to RunDependenciesChanged then CountRule should be recomputed
    -- result should be changed 0, build 1
    _res1 <- shakeRunDatabaseForKeys (Just [child]) db [apply1 CountRule] -- count = 2
    -- since child changed = parent build
    -- instruct to RunDependenciesSame then CountRule should not be recomputed
    -- result should be changed 0, build 1
    _res3 <- shakeRunDatabaseForKeys (Just [parent]) db [apply1 CountRule] -- count = 2
    -- invariant child changed = parent build should remains after RunDependenciesSame
    -- this used to be a bug, with additional computation, see https://github.com/haskell/haskell-language-server/pull/4238
    _res3 <- shakeRunDatabaseForKeys (Just [parent]) db [apply1 CountRule] -- count = 2
    c1 <- readMVar count1
    c1 `shouldBe` 2
  describe "apply1" $ do
    it "computes a rule with no dependencies" $ do
      db <- shakeNewDatabase shakeOptions ruleUnit
      res <- shakeRunDatabase db $
        pure $ apply1 (Rule @())
      res `shouldBe` [()]
    it "computes a rule with one dependency" $ do
      db <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      res <- shakeRunDatabase db $ pure $ apply1 Rule
      res `shouldBe` [True]
    it "tracks direct dependencies" $ do
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
      resultDeps res `shouldBe` ResultDeps [singletonKeySet $ newKey (Rule @())]
    it "tracks reverse dependencies" $ do
      db@(ShakeDatabase _ _ Database {..}) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just KeyDetails {..} <- atomically $ STM.lookup (newKey (Rule @())) databaseValues
      keyReverseDeps `shouldBe` singletonKeySet (newKey theKey)
    it "rethrows exceptions" $ do
      db <- shakeNewDatabase shakeOptions $ addRule $ \(Rule :: Rule ()) _old _mode -> error "boom"
      let res = shakeRunDatabase db $ pure $ apply1 (Rule @())
      res `shouldThrow` anyErrorCall
    it "computes a rule with branching dependencies does not invoke phantom dependencies #3423" $ do
      cond <- C.newMVar True
      count <- C.newMVar 0
      (ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleCond cond
        ruleSubBranch count
        ruleWithCond
      -- build the one with the condition True
      -- This should call the SubBranchRule once
      -- cond rule would return different results each time
      res0 <- build theDb emptyStack [BranchedRule]
      snd res0 `shouldBe` [1 :: Int]
      incDatabase theDb Nothing
      -- build the one with the condition False
      -- This should not call the SubBranchRule
      res1 <- build theDb emptyStack [BranchedRule]
      snd res1 `shouldBe` [2 :: Int]
     -- SubBranchRule should be recomputed once before this (when the condition was True)
      countRes <- build theDb emptyStack [SubBranchRule]
      snd countRes `shouldBe` [1 :: Int]

  describe "applyWithoutDependency" $ it "does not track dependencies" $ do
    db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
      ruleUnit
      addRule $ \Rule _old _mode -> do
          [()] <- applyWithoutDependency [Rule]
          return $ RunResult ChangedRecomputeDiff "" True $ return ()

    let theKey = Rule @Bool
    res <- shakeRunDatabase db $
      pure $ applyWithoutDependency [theKey]
    res `shouldBe` [[True]]
    Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
    resultDeps res `shouldBe` UnknownDeps

  describe "Closing escaped rule computations" $ do
    it "does not leave a late async alive outside its closed AIO scope" $ do
      bodyStarted <- newEmptyMVar
      releaseBody <- newEmptyMVar
      bodyFinished <- newEmptyMVar
      ShakeDatabase _ _ theDb@Database{..} <- shakeNewDatabase shakeOptions $
        addRule $ \(Rule :: Rule ()) _old _mode -> do
          liftIO $
            (C.putMVar bodyStarted () >> C.takeMVar releaseBody)
              `finally` C.putMVar bodyFinished ()
          pure $ RunResult ChangedRecomputeDiff "" () (pure ())

      -- The first build publishes a lazy 'Running' force but cannot reach its
      -- forcing phase while traversing this infinite list.
      withAsync (build theDb emptyStack $ repeat (Rule @())) $ \firstBuild -> do
        atomically $ do
          details <- STM.lookup (newKey (Rule @())) databaseValues
          case details of
            Just KeyDetails{keyStatus = Running{}} -> pure ()
            _                                      -> retry

        -- A concurrent build selects that Running force, then parks while the
        -- first build still owns an open AIO scope.
        reachedSecondKey <- newEmptyMVar
        releaseSecondKey <- newEmptyMVar
        withAsync
          (build theDb emptyStack
            [Rule @(), blockSecondKey reachedSecondKey releaseSecondKey (Rule @())]) $ \secondBuild -> do
              C.takeMVar reachedSecondKey
              -- Close the force's original scope before the already-running
              -- second build proceeds to force what it selected.
              cancel firstBuild
              C.putMVar releaseSecondKey ()
              started <- timeout 1000000 $ C.takeMVar bodyStarted
              -- Cancelling the waiter must not reveal a child that survived
              -- teardown in the already-closed first scope.
              cancel secondBuild
              finished <- C.tryReadMVar bodyFinished
              let outOfScopeAlive = isJust started && isNothing finished
              -- Let an upstream orphan finish before reporting RED, so the
              -- regression test itself never leaks a thread.
              case (started, finished) of
                (Just (), Nothing) -> do
                  C.putMVar releaseBody ()
                  C.takeMVar bodyFinished
                _ -> pure ()
              outOfScopeAlive `shouldBe` False

    it "self-terminates when nested work observes its AIO scope is closed" $ do
      ShakeDatabase _ _ theDb@Database{..} <-
        shakeNewDatabase shakeOptions scopeRules
      -- Give the parent two recorded dependencies so refreshing it takes the
      -- multi-spawn admission path.
      void $ build theDb emptyStack [ScopeParent]
      incDatabase theDb Nothing

      -- Publish a lazy refresh tied to this first build's AIO scope.
      withAsync (build theDb emptyStack $ repeat ScopeParent) $ \firstBuild -> do
        atomically $ do
          details <- STM.lookup (newKey ScopeParent) databaseValues
          case details of
            Just KeyDetails{keyStatus = Running{}} -> pure ()
            _                                      -> retry

        reachedSecondKey <- newEmptyMVar
        releaseSecondKey <- newEmptyMVar
        withAsync
          (build theDb emptyStack
            [ ScopeParent
            , blockSecondKey reachedSecondKey releaseSecondKey ScopeLeft
            ]) $ \secondBuild -> do
              C.takeMVar reachedSecondKey
              cancel firstBuild
              C.putMVar releaseSecondKey ()
              secondResult <- timeout 1000000 $ waitCatch secondBuild
              -- Cleanup only matters for the old waiting behavior; the fixed
              -- build has already raised 'AsyncCancelled' in its own thread.
              cancel secondBuild
              let selfTerminated = case secondResult of
                    Just (Left err) ->
                      isJust (fromException err :: Maybe AsyncCancelled)
                    _ -> False
              selfTerminated `shouldBe` True
