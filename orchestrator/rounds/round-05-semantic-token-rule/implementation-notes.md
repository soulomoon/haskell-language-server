### Changes Made
- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs: restored the target-branch ordinary `define` rule for `GetSemanticTokens`, removing `defineEarlyCutoff`, old-value reuse, `IsFileOfInterest` gating, and the imports/extensions needed only by that path.
- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs: restored target-branch semantic-token logging constructors and `Pretty` output: `LogDependencyError PluginError` and `LogNoVF`.

### Tests
- plugins/hls-semantic-tokens-plugin/test/SemanticTokensTest.hs: existing semantic-token suite passed under GHC 9.12.2; all 30 tests passed.
- Validation commands run:
  - `git status --short --branch`
  - `git diff --check`
  - `git diff --name-status origin/codex/hls-graph-runtime-engine HEAD -- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`
  - `git diff --exit-code origin/codex/hls-graph-runtime-engine -- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`
  - `ghcup run --ghc 9.12.2 -- cabal build haskell-language-server:lib:hls-semantic-tokens-plugin`
  - `ghcup run --ghc 9.12.2 -- cabal test haskell-language-server:test:hls-semantic-tokens-plugin-tests`
  - `ghcup run --ghc 9.12.2 -- cabal build`

### Notes
The working tree versions of the two assigned semantic-token files match `origin/codex/hls-graph-runtime-engine`; no compile-only adaptation remains. The required `git diff --name-status origin/codex/hls-graph-runtime-engine HEAD -- ...` comparison still reports the two files as branch-level diffs because `HEAD` contains the pre-restore implementation, while the final working tree comparison with `git diff --exit-code origin/codex/hls-graph-runtime-engine -- ...` is clean.
