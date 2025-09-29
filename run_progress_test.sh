#!/bin/bash

set -e
# pattern="edit header"

# test_target="func-test"
# pattern="sends indefinite progress notifications"
test_target="ghcide-tests"
pattern="lower-case drive"
# HLS_TEST_LOG_STDERR=1
NumberOfRuns=1
  # TASTY_PATTERN="sends indefinite progress notifications" cabal test func-test
  # TASTY_PATTERN="notification handlers run in priority order" cabal test ghcide-tests


cabal build $test_target
targetBin=$(find dist-newstyle -type f -name $test_target)
for i in {1..$NumberOfRuns}; do
  echo "Run #$i"
  # TASTY_PATTERN=$pattern HLS_TEST_LOG_STDERR=$HLS_TEST_LOG_STDERR HLS_TEST_HARNESS_STDERR=1 $targetBin
  TASTY_PATTERN=$pattern HLS_TEST_HARNESS_STDERR=1 $targetBin
done
