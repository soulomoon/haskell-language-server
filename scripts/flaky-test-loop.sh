# # Run 50 iterations
# cabal run scripts/flaky-test-loop.hs -- 50

# # Same, via env
# MAX_ITER=50 cabal run scripts/flaky-test-loop.hs

# # One tasty pattern, defaults to ghcide-tests
# TEST_PATTERNS='open close' MAX_ITER=50 cabal run scripts/flaky-test-loop.hs

# # Multiple test binaries/patterns
# TEST_PATTERNS='ghcide-tests::open close,func-test::progress' MAX_ITER=50 cabal run scripts/flaky-test-loop.hs

# Use the checked-in pattern file
PATTERN_FILE=scripts/flaky-test-patterns.txt MAX_ITER=50 cabal run scripts/flaky-test-loop.hs

# Skip the initial cabal build if already built
# NO_BUILD_ONCE=1 TEST_PATTERNS='ghcide-tests::open close' cabal run scripts/flaky-test-loop.hs
