### Goal

Restore semantic-token rule behavior to match `origin/codex/hls-graph-runtime-engine` for the selected plugin-rule parity suspect, limited to the ordinary `define` rule behavior versus the current early-cutoff/file-of-interest-gated old-value behavior, plus direct compile fallout only.

### Approach

Use a sequential, two-file semantic-token restore. The current branch differs from `origin/codex/hls-graph-runtime-engine` in this target area only at:

- `plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs`
- `plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`

In `Internal.hs`, restore `getSemanticTokensRule` to the target branch's ordinary `define` implementation that always computes `GetSemanticTokens` through `GetHieAst`, stale `GetDocMap`, AST lookup, virtual file lookup, and `computeRangeHsSemanticTokenTypeList`. Remove the current `defineEarlyCutoff` / `RuleWithOldValue` / `IsFileOfInterest` / `currentValue old` path and the imports and language extension needed only by that path.

In `Types.hs`, restore the semantic-token log constructors and `Pretty` output to the target branch shape used by the ordinary rule: `LogDependencyError PluginError` and `LogNoVF`, with the target branch messages. If restoring the exact target text exposes a compile error caused by nearby branch API drift, keep the adaptation inside the semantic-token plugin files and record the exact intentional diff from target branch in the implementation notes.

Do not touch HLint, eval-plugin, residual runtime diffs, benchmark CI, or unrelated formatting/refactors. Do not weaken tests.

### Steps

1. Confirm the starting state and target branch:
   - `git status --short --branch`
   - `git rev-parse --short HEAD origin/codex/hls-graph-runtime-engine`
2. Inspect the selected diff before editing:
   - `git diff --name-status origin/codex/hls-graph-runtime-engine HEAD -- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`
   - `git diff origin/codex/hls-graph-runtime-engine HEAD -- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`
3. Edit `Internal.hs` to match the target branch semantic-token rule behavior:
   - Remove `LambdaCase`.
   - Restore the explicit `Development.IDE` import list containing `define`.
   - Remove imports for `IsFileOfInterest`, `IsFileOfInterestResult`, `RuleBody`, and `currentValue`.
   - Replace the `defineEarlyCutoff ... RuleWithOldValue` body with the target branch `define ... handleError` body.
   - Restore `withExceptT LogDependencyError` and `handleMaybeM LogNoVF` call sites.
4. Edit `Types.hs` to match the target branch logging API used by that restored rule:
   - Change `LogDependencyError NormalizedFilePath PluginError` back to `LogDependencyError PluginError`.
   - Change `LogNoVF NormalizedFilePath` back to `LogNoVF`.
   - Restore the corresponding `Pretty` cases to the target branch text.
5. Re-check that no out-of-scope files were modified. If a compile-only adaptation is needed, keep it in the same semantic-token plugin area and document why target-exact text cannot compile on this branch.
6. Do not write `worker-plan.json`; the roadmap and selection require a serial one-suspect experiment, and the edits are tightly coupled in one plugin rule/logging surface.

### Verification

Run these checks after implementation:

1. Worktree and hygiene:
   - `git status --short --branch`
   - `git diff --check`
2. Target-branch comparison for touched semantic-token files:
   - `git diff --name-status origin/codex/hls-graph-runtime-engine HEAD -- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`
   - `git diff --exit-code origin/codex/hls-graph-runtime-engine -- plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`
   - If the second command fails because of direct compile fallout, include the remaining diff in the implementation report and explain why each hunk is compile-only.
3. Focused semantic-token build/test:
   - `ghcup run --ghc 9.12.2 -- cabal build haskell-language-server:lib:hls-semantic-tokens-plugin`
   - If practical in the local environment, also run `ghcup run --ghc 9.12.2 -- cabal test haskell-language-server:test:hls-semantic-tokens-plugin-tests`.
4. Full branch build gate:
   - `ghcup run --ghc 9.12.2 -- cabal build`

After the implementer commits and pushes, stop for operator benchmark feedback rather than starting HLint, eval-plugin, residual triage, or benchmark CI cleanup.

### Worker Fan-Out

Worker fan-out is not used. The round is serial by roadmap rule, and the implementation ownership is a single coupled semantic-token rule/logging change.
