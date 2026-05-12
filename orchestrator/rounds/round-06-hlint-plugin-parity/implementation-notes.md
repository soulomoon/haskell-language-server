### Changes Made
- `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`: restored `GetHlintSettings` to call `liftIO $ argsSettings flags` directly, matching `origin/codex/hls-graph-runtime-engine` for the selected HLint settings-rule behavior. The existing `Control.Exception` import was left unchanged per plan.
- `orchestrator/rounds/round-06-hlint-plugin-parity/implementation-notes.md`: recorded scoped implementation evidence and verification outcomes for this round.

### Tests
- No test files changed. This round restores one selected production behavior to target-branch parity.
- `git status --short --branch`: passed; showed branch `orchestrator/round-06-hlint-plugin-parity`, modified HLint file, and the round artifact directory.
- `git diff --check`: passed with no output.
- `git diff --exit-code origin/codex/hls-graph-runtime-engine -- plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`: passed with no output; the scoped HLint file matches the target branch.
- `ghcup run --ghc 9.12.2 -- cabal build haskell-language-server:hls-hlint-plugin`: passed.
- `ghcup run --ghc 9.12.2 -- cabal build`: passed.

### Notes
No direct compile fallout occurred from the HLint restoration. No class-plugin, eval-plugin, runtime, benchmark, or unrelated cleanup changes were made. Local builds do not claim benchmark parity; the roadmap still requires pushed benchmark workflow feedback to decide whether this suspected cause is ruled in or out.
