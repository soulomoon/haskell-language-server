echo "" > test.log
echo "" > test.err.log
for i in {1..500}; do
    /Users/ares/src/soulomoonHLS/dist-newstyle/build/aarch64-osx/ghc-9.4.8/haskell-language-server-2.6.0.0/t/hls-eval-plugin-tests/build/hls-eval-plugin-tests/hls-eval-plugin-tests -p "/i behaves exactly the same as/" >> test.log 2>> test.err.log
    echo "Iteration: $i" >> test.log
    echo "Iteration: $i" >> test.err.log
done
