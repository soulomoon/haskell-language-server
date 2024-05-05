# recommended to build test binary separately and then run it in a loop (to avoid running cabal test in a loop)
# cabal build hls-eval-plugin-tests
# find dist-newstyle -name hls-eval-plugin-tests -type f
# # ./dist-newstyle/build/x86_64-linux/ghc-9.4.8/haskell-language-server-2.7.0.0/t/hls-eval-plugin-tests/build/hls-eval-plugin-tests/hls-eval-plugin-tests
# # Run tests in a loop
for i in {1..500}; do
    echo "Iteration $i" &&
    # LSP_TEST_LOG_MESSAGES=0 LSP_TEST_LOG_STDERR=0 TASTY_PATTERN="Testing PluginError order" $(find dist-newstyle -name ghcide-tests -type f | head -n 1) \
    LSP_TEST_LOG_MESSAGES=0 LSP_TEST_LOG_STDERR=0 TASTY_PATTERN="notification handlers run in priority order" $(find dist-newstyle -name ghcide-tests -type f | head -n 1) \
    || {
        # line_count=$(wc -l < test.txt)
        # if (( line_count > 100 )); then
        #     echo "Warning: More than 100 lines in test.txt at iteration $i"
        #     break
        # fi
        echo "Warning: error at iteration $i"
        break
        }; done

