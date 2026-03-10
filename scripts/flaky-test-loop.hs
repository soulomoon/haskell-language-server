#!/usr/bin/env cabal
{- cabal:
build-depends:
    base >= 4.14 && < 5
  , text
  , regex-tdfa
  , unix
  , directory
  , process
ghc-options: -Wall -threaded
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Loop running HLS tasty tests until a Broken pipe or test failure is observed.
--
-- Run with: cabal run scripts/flaky-test-loop.hs
--
-- Architecture: Parse -> Plan -> Run
--   1. Parse: Read configuration and test specifications (pure)
--   2. Plan: Build execution plan with all information needed to run (pure)
--   3. Run: Execute the plan, performing IO effects

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception  (IOException, try)
import           Control.Monad      (foldM, unless, when)
import           Data.Char          (isAlphaNum, isSpace)
import           Data.List          (isInfixOf)
import           Data.Maybe         (fromMaybe)
import           Data.String        (IsString)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Directory   (createDirectoryIfMissing, doesFileExist)
import           System.Environment (getArgs, lookupEnv)
import           System.Exit        (ExitCode (..), exitWith)
import           System.IO          (IOMode (..), hPutStrLn, stderr, withFile)
import           System.Posix.Env   (setEnv)
import           System.Process     (readProcess, readProcessWithExitCode)
import           Text.Regex.TDFA    ((=~))

-- =============================================================================
-- DOMAIN TYPES (Shared across all stages)
-- =============================================================================

-- | Name of a test binary (e.g., "ghcide-tests")
newtype BinaryName = BinaryName { unBinaryName :: String }
  deriving (Show, Eq, Ord, IsString)

-- | A tasty test pattern (e.g., "open close")
newtype TestPattern = TestPattern { unTestPattern :: String }
  deriving (Show, Eq, IsString)

-- | A file path for log output
newtype LogPath = LogPath { unLogPath :: FilePath }
  deriving (Show, Eq, IsString)

-- | Sanitized slug for log filenames
newtype Slug = Slug { unSlug :: String }
  deriving (Show, Eq, IsString)

-- | Iteration counter (1-based)
newtype Iteration = Iteration { unIteration :: Int }
  deriving (Show, Eq, Ord, Num)

-- | Maximum number of iterations to run
newtype MaxIter = MaxIter { unMaxIter :: Int }
  deriving (Show, Eq, Ord, Num, Read)

-- | Sleep duration in seconds between iterations
newtype SleepSecs = SleepSecs { unSleepSecs :: Int }
  deriving (Show, Eq, Ord, Num)

-- | Progress reporting interval
newtype ShowEvery = ShowEvery { unShowEvery :: Int }
  deriving (Show, Eq, Ord, Num)

-- | Log slot (0 or 1) for alternating log files
newtype LogSlot = LogSlot { unLogSlot :: Int }
  deriving (Show, Eq)

-- =============================================================================
-- PARSE STAGE: Input parsing and validation (Pure)
-- =============================================================================

-- | Where test patterns come from
data PatternSource
  = FromEnvVar String
  | FromFile FilePath
  | DefaultPattern
  deriving (Show, Eq)

-- | Raw configuration from environment/args
data RawConfig = RawConfig
  { rawMaxIter       :: MaxIter
  , rawSleepSecs     :: SleepSecs
  , rawShowEvery     :: ShowEvery
  , rawLogStderr     :: LogStderr
  , rawDebugDetect   :: DebugDetect
  , rawNoBuildOnce   :: NoBuildOnce
  , rawPatternSource :: PatternSource
  }

data LogStderr = LogStderrEnabled | LogStderrDisabled deriving (Show, Eq)
data DebugDetect = DebugDetectEnabled | DebugDetectDisabled deriving (Show, Eq)
data NoBuildOnce = SkipBuild | DoBuild deriving (Show, Eq)

-- | A single test item specification
data TestSpec = TestSpec
  { specBinary  :: BinaryName
  , specPattern :: TestPattern
  } deriving (Show, Eq)

-- | Result of parsing stage
data ParseResult
  = ParseSuccess RawConfig [TestSpec]
  | ParseError ParseErrorDetail

data ParseErrorDetail
  = InvalidMaxIter String
  | NoPatternsProvided
  | PatternFileNotFound FilePath
  deriving (Show, Eq)

-- Pure parsing functions

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

parseBool :: String -> Bool
parseBool s = trim s `elem` ["1", "true", "True", "TRUE", "yes", "YES", "on", "ON"]

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = case break (== c) s of
  (l, [])  -> [l]
  (l, _:r) -> l : splitOn c r

breakOn :: String -> String -> (String, String)
breakOn sep s = case findSep 0 s of
  Nothing -> (s, "")
  Just i  -> (take i s, drop i s)
  where
    len = length sep
    findSep _ [] = Nothing
    findSep i rest
      | take len rest == sep = Just i
      | null rest = Nothing
      | otherwise = findSep (i + 1) (tail rest)

-- | Parse a single test pattern entry
parseTestEntry :: String -> TestSpec
parseTestEntry entry
  | null trimmed = TestSpec (BinaryName "") (TestPattern "")
  | "::" `isInfixOf` trimmed =
      let (bin, pat) = breakOn "::" trimmed
      in TestSpec (BinaryName bin) (TestPattern $ if null pat then "" else drop 2 pat)
  | otherwise = TestSpec (BinaryName "ghcide-tests") (TestPattern trimmed)
  where trimmed = trim entry

-- | Parse comma-separated patterns from environment variable
parseEnvPatterns :: String -> [TestSpec]
parseEnvPatterns = map parseTestEntry . splitOn ','

-- | Parse patterns from file content (pure, no IO)
parseFileContent :: String -> [TestSpec]
parseFileContent content = map parseLine (lines content)
  where
    parseLine line =
      let trimmed = trim line
      in if null trimmed || head trimmed == '#'
         then TestSpec (BinaryName "") (TestPattern "")
         else parseTestEntry trimmed

-- | Determine pattern source from environment (pure logic, takes Maybe values)
determinePatternSource :: Maybe String -> Maybe String -> PatternSource
determinePatternSource mPatterns mFile = case (mPatterns, mFile) of
  (Just tp, _) | not (null tp) -> FromEnvVar tp
  (_, Just pf)                 -> FromFile pf
  _                            -> DefaultPattern

-- | Parse raw configuration from environment (effectful boundary)
parseConfigFromEnv :: IO RawConfig
parseConfigFromEnv = do
  args <- getArgs
  let positionalMaxIter = case args of
        (x:_) -> MaxIter <$> readMaybe x
        _     -> Nothing

  maxIterVal <- lookupEnvInt "MAX_ITER" (MaxIter 1000)
  let effectiveMaxIter = fromMaybe maxIterVal positionalMaxIter

  patternSource <- determinePatternSource
    <$> lookupEnv "TEST_PATTERNS"
    <*> lookupEnv "PATTERN_FILE"

  RawConfig
    <$> pure effectiveMaxIter
    <*> (SleepSecs <$> lookupEnvInt "SLEEP_SECS" 0)
    <*> (ShowEvery <$> lookupEnvInt "SHOW_EVERY" 1)
    <*> lookupEnvBool "LOG_STDERR" LogStderrEnabled LogStderrDisabled
    <*> lookupEnvBool "DEBUG_DETECT" DebugDetectEnabled DebugDetectDisabled
    <*> lookupEnvBool "NO_BUILD_ONCE" SkipBuild DoBuild
    <*> pure patternSource
  where
    lookupEnvInt var def = fromMaybe def . (readMaybe =<<) <$> lookupEnv var
    lookupEnvBool var t f = do
      mval <- lookupEnv var
      pure $ case mval of
        Just s | parseBool s -> t
        _                    -> f

-- | Full parse stage - returns pure result to be handled
parseStage :: IO ParseResult
parseStage = do
  rawConfig <- parseConfigFromEnv

  -- Parse test specs based on source (requires IO for file reading)
  specs <- case rawPatternSource rawConfig of
    FromEnvVar tp -> pure $ parseEnvPatterns tp
    FromFile fp -> do
      exists <- doesFileExist fp
      if exists
        then parseFileContent <$> readFile fp
        else pure []  -- Will be caught by validation
    DefaultPattern -> pure [TestSpec (BinaryName "ghcide-tests") (TestPattern "open close")]

  -- Validate
  let validSpecs = filter (not . null . unTestPattern . specPattern) specs

  pure $ case () of
    _ | null validSpecs -> ParseError NoPatternsProvided
    _                   -> ParseSuccess rawConfig validSpecs

-- =============================================================================
-- PLAN STAGE: Build execution plan (Pure)
-- =============================================================================

-- | A single test run in the plan
data PlannedTest = PlannedTest
  { plannedBinary  :: BinaryName
  , plannedPattern :: TestPattern
  , plannedSlug    :: Slug
  , plannedLogBase :: FilePath  -- ^ Base path without slot suffix
  } deriving (Show, Eq)

-- | Complete execution plan
data ExecutionPlan = ExecutionPlan
  { planMaxIter         :: MaxIter
  , planSleepSecs       :: SleepSecs
  , planShowEvery       :: ShowEvery
  , planLogStderr       :: LogStderr
  , planDebugDetect     :: DebugDetect
  , planSkipBuild       :: Bool
  , planTests           :: [PlannedTest]
  , planBinariesToBuild :: [BinaryName]  -- ^ Unique binaries that need building
  } deriving (Show)

sanitizeSlug :: BinaryName -> TestPattern -> Slug
sanitizeSlug (BinaryName bin) (TestPattern pat) =
  let combined = bin ++ "-" ++ pat
      replaced = map (\c -> if isValidChar c then c else '-') combined
      isValidChar c = isAlphaNum c || c == '.' || c == '_' || c == '-'
      trimmed' = dropWhile (== '-') replaced
      trimmed'' = reverse $ dropWhile (== '-') $ reverse trimmed'
  in Slug $ if null trimmed'' then "pattern" else trimmed''

mkLogPath :: Slug -> LogSlot -> LogPath
mkLogPath slug (LogSlot slot) =
  LogPath $ "test-logs/" ++ unSlug slug ++ "-loop-" ++ show slot ++ ".log"

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (/= x) xs)

-- | Build execution plan from parsed configuration (pure function)
buildPlan :: RawConfig -> [TestSpec] -> ExecutionPlan
buildPlan RawConfig{..} specs = ExecutionPlan
  { planMaxIter = rawMaxIter
  , planSleepSecs = rawSleepSecs
  , planShowEvery = rawShowEvery
  , planLogStderr = rawLogStderr
  , planDebugDetect = rawDebugDetect
  , planSkipBuild = case rawNoBuildOnce of
      SkipBuild -> True
      DoBuild   -> False
  , planTests = map planTest specs
  , planBinariesToBuild = unique $ map specBinary specs
  }
  where
    planTest :: TestSpec -> PlannedTest
    planTest TestSpec{..} = PlannedTest
      { plannedBinary = specBinary
      , plannedPattern = specPattern
      , plannedSlug = sanitizeSlug specBinary specPattern
      , plannedLogBase = "test-logs/" ++ unSlug (sanitizeSlug specBinary specPattern) ++ "-loop-"
      }

-- | Decide whether to show iteration header
shouldShowHeader :: Iteration -> ShowEvery -> Bool
shouldShowHeader (Iteration n) (ShowEvery e)
  | n == 1 = True
  | e > 0 && n `mod` e == 0 = True
  | otherwise = False

-- | Calculate log slot from iteration
mkLogSlot :: Iteration -> LogSlot
mkLogSlot (Iteration n) = LogSlot (n `mod` 2)

-- =============================================================================
-- RUN STAGE: Execute the plan (Effectful)
-- =============================================================================

data Issue = BrokenPipe | TestFailure deriving (Show, Eq)

data TestOutcome = IssueFound Issue | NoIssue deriving (Show, Eq)

logStderrToString :: LogStderr -> String
logStderrToString LogStderrEnabled  = "1"
logStderrToString LogStderrDisabled = "0"

issueToMessage :: Issue -> String
issueToMessage BrokenPipe  = "Broken pipe reproduced"
issueToMessage TestFailure = "Test failure detected"

-- Logging helpers
logMsg :: String -> IO ()
logMsg msg = hPutStrLn stderr $ "[loop] " ++ msg

logError :: String -> IO ()
logError msg = hPutStrLn stderr $ "[loop][error] " ++ msg

logDebug :: DebugDetect -> String -> IO ()
logDebug DebugDetectEnabled msg = logMsg $ "[debug] " ++ msg
logDebug DebugDetectDisabled _  = pure ()

-- Binary resolution with caching
type BinCache = [(BinaryName, FilePath)]

resolveBinary :: BinCache -> BinaryName -> IO (FilePath, BinCache)
resolveBinary cache binName = case lookup binName cache of
  Just path -> pure (path, cache)
  Nothing -> do
    result <- try $ readProcess "cabal" ["list-bin", unBinaryName binName, "--verbose=0"] ""
    case result of
      Left (_ :: IOException) -> do
        logError $ "Unable to locate binary for '" ++ unBinaryName binName ++ "' via 'cabal list-bin'."
        logError $ "Try running 'cabal build " ++ unBinaryName binName ++ "' to ensure the target exists."
        exitWith (ExitFailure 2)
      Right output ->
        let path = trim output
        in pure (path, (binName, path) : cache)

-- Build step
runBuildStep :: ExecutionPlan -> IO ()
runBuildStep plan | planSkipBuild plan = pure ()
runBuildStep plan = do
  let bins = planBinariesToBuild plan
  if null bins
    then pure ()
    else do
      logMsg $ "Building test targets: " ++ unwords (map unBinaryName bins)
      (exitCode, _, stderr') <- readProcessWithExitCode "cabal"
        ("build" : map unBinaryName bins) ""
      case exitCode of
        ExitFailure _ -> do
          unless (null stderr') $ hPutStrLn stderr stderr'
          logError "Build failed. Cannot proceed with tests."
          exitWith (ExitFailure 2)
        ExitSuccess -> logMsg "Build succeeded."

-- Check log file for issues
checkLogForIssues :: DebugDetect -> LogPath -> Iteration -> TestPattern -> IO TestOutcome
checkLogForIssues debug logPath (Iteration n) pat = do
  content <- TIO.readFile (unLogPath logPath)
  let text = T.unpack content

  if text =~ brokenPipePattern
    then pure $ IssueFound BrokenPipe
    else if text =~ testFailedPattern
      then pure $ IssueFound TestFailure
      else do
        logDebug debug $ "No issues in iteration " ++ show n ++
                        " (pattern='" ++ unTestPattern pat ++ "')."
        pure NoIssue
  where
    brokenPipePattern :: String
    brokenPipePattern = "Broken pipe"
    testFailedPattern :: String
    testFailedPattern = "tests failed|timeout"

-- Report an issue and exit
reportIssue :: LogPath -> Iteration -> TestPattern -> Issue -> IO ()
reportIssue logPath (Iteration n) pat issue = do
  let msg = issueToMessage issue

  appendToLog $ "[loop] " ++ msg ++ " in iteration " ++ show n ++
                " for pattern '" ++ unTestPattern pat ++ "'. Stopping."
  appendToLog $ "[loop] Log file: " ++ unLogPath logPath ++ " (abs: " ++ unLogPath logPath ++ ")"

  logMsg $ msg ++ " in iteration " ++ show n ++
           " for pattern '" ++ unTestPattern pat ++ "'. Stopping."
  logMsg $ "Log file: " ++ unLogPath logPath ++ " (abs: " ++ unLogPath logPath ++ ")"
  logMsg "--- Tail (last 60 lines) ---"

  -- Display last 60 lines
  content <- TIO.readFile (unLogPath logPath)
  let lines' = T.lines content
      last60 = drop (length lines' - 60) lines'
  mapM_ TIO.putStrLn last60
  where
    appendToLog txt = withFile (unLogPath logPath) AppendMode $ \h -> hPutStrLn h txt

-- Execute a single test
runTest :: ExecutionPlan -> BinCache -> PlannedTest -> Iteration -> LogSlot -> IO BinCache
runTest plan cache test iteration logSlot = do
  let logPath = LogPath (plannedLogBase test ++ show (unLogSlot logSlot) ++ ".log")

  -- Show header if needed
  when (shouldShowHeader iteration (planShowEvery plan)) $ do
    let header = "[loop] Iteration " ++ show (unIteration iteration) ++
                 " pattern='" ++ unTestPattern (plannedPattern test) ++ "' -> " ++ unLogPath logPath
    hPutStrLn stderr header
    appendFile (unLogPath logPath) (header ++ "\n")

  -- Resolve binary
  (binPath, cache') <- resolveBinary cache (plannedBinary test)

  -- Set environment
  setEnv "HLS_TEST_LOG_STDERR" (logStderrToString $ planLogStderr plan) True
  setEnv "HLS_TEST_HARNESS_STDERR" (logStderrToString $ planLogStderr plan) True
  setEnv "TASTY_NUM_THREADS" "1" True
  setEnv "TASTY_PATTERN" (unTestPattern $ plannedPattern test) True

  -- Run test
  (_, stdout', stderr') <- readProcessWithExitCode binPath
    ["+RTS", "-l", "-olhlint.eventlog", "-RTS"] ""

  -- Write log
  TIO.writeFile (unLogPath logPath) (T.pack (stdout' ++ stderr'))

  -- Check for issues
  outcome <- checkLogForIssues (planDebugDetect plan) logPath iteration (plannedPattern test)
  case outcome of
    IssueFound issue -> do
      reportIssue logPath iteration (plannedPattern test) issue
      exitWith (ExitFailure 1)
    NoIssue -> pure cache'

-- Loop state
data LoopState = LoopState
  { stateIteration :: Iteration
  , stateBinCache  :: BinCache
  }

-- Run one iteration over all tests
runIteration :: ExecutionPlan -> LoopState -> IO LoopState
runIteration plan (LoopState iter cache) = do
  let iter' = iter + 1
      logSlot = mkLogSlot iter'

  -- Run all tests
  cache' <- foldM (\c test -> runTest plan c test iter' logSlot) cache (planTests plan)

  -- Progress report
  when (unShowEvery (planShowEvery plan) > 0 &&
        unIteration iter' `mod` unShowEvery (planShowEvery plan) == 0) $
    logMsg $ "Progress: Completed " ++ show (unIteration iter') ++ " iterations without detecting issues."

  -- Sleep
  when (unSleepSecs (planSleepSecs plan) > 0) $ do
    logMsg $ "Sleeping " ++ show (unSleepSecs (planSleepSecs plan)) ++ "s"
    threadDelay (unSleepSecs (planSleepSecs plan) * 1000000)

  pure $ LoopState iter' cache'

-- Main execution loop
runExecutionLoop :: ExecutionPlan -> IO ()
runExecutionLoop plan = go (LoopState 0 [])
  where
    go state
      | unIteration (stateIteration state) < unMaxIter (planMaxIter plan) = do
          state' <- runIteration plan state
          go state'
      | otherwise = do
          logMsg $ "Reached MAX_ITER=" ++ show (unMaxIter $ planMaxIter plan) ++
                   " without reproducing issues."
          exitWith ExitSuccess

-- =============================================================================
-- MAIN: Orchestrate Parse -> Plan -> Run
-- =============================================================================

main :: IO ()
main = do
  -- Stage 1: Parse
  parseResult <- parseStage

  case parseResult of
    ParseError err -> do
      case err of
        NoPatternsProvided ->
          logError "No test entries provided (via PATTERN_FILE or TEST_PATTERNS)."
        InvalidMaxIter s ->
          logError $ "Invalid MAX_ITER value: " ++ s
        PatternFileNotFound fp ->
          logError $ "Pattern file not found: " ++ fp
      exitWith (ExitFailure 2)

    ParseSuccess rawConfig specs -> do
      -- Stage 2: Plan (pure)
      let plan = buildPlan rawConfig specs

      -- Stage 3: Run (effectful)
      createDirectoryIfMissing True "test-logs"
      runBuildStep plan
      logMsg "Starting flaky test loop"
      runExecutionLoop plan
