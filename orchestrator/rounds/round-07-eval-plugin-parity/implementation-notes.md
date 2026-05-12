### Changes Made
- `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs`: restored exactly from `origin/codex/hls-graph-runtime-engine` for the selected eval-plugin command handler parity experiment. The resulting scoped diff against the target branch is empty. No direct compile/import adaptation was needed.

### Tests
- `git status --short --branch`: ran before editing; output showed branch `orchestrator/round-07-eval-plugin-parity` with only `?? orchestrator/rounds/round-07-eval-plugin-parity/` untracked.
- `git diff --check`: passed with no output after restoration.
- `git diff --name-only`: after source restoration, output was only `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs`.
- `git diff origin/codex/hls-graph-runtime-engine -- plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs`: passed with no output after restoration.
- `ghcup run --ghc 9.12.2 -- cabal build haskell-language-server:hls-eval-plugin`: passed. It built `hls-graph`, `hls-plugin-api`, `ghcide`, and `lib:hls-eval-plugin`; warnings were emitted in existing modules, but no errors.
- `ghcup run --ghc 9.12.2 -- cabal test hls-eval-plugin-tests`: failed after 66 of 68 tests passed. The failures were `eval.Property checking` and `eval.Property checking with exception`; both actual outputs reported `-- Add QuickCheck to your cabal dependencies to run this test.` instead of the expected QuickCheck property result/failure. Cabal wrote the test log to `./dist-newstyle/build/aarch64-osx/ghc-9.12.2/haskell-language-server-2.14.0.0/t/hls-eval-plugin-tests/test/haskell-language-server-2.14.0.0-hls-eval-plugin-tests.log`.
- `ghcup run --ghc 9.12.2 -- cabal build`: passed. The full build compiled the repo-wide component set, including executables, benchmark, and test executables.

### Notes
The only production change is import-shape/order parity in the selected eval handler file. The focused test failure is not direct compile fallout from restoring `Handlers.hs`; it is runtime test output from the eval plugin's property-checking fixtures when QuickCheck is not available to the evaluated test project. Per the round boundary, no test fixture, dependency, class-plugin, runtime/session, benchmark CI, or unrelated files were changed.
