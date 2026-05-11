### Changes Made
- ghcide/session-loader/Development/IDE/Session.hs: restored `getOptionsLoop` pending-barrier ordering to match `origin/codex/hls-graph-runtime-engine`, so test mode waits with `waitForSessionLoaderPendingBarrier` before dequeuing with `S.readQueue`.

### Tests
- No test files changed: this round intentionally restores one session-loader ordering hunk for benchmark comparison.
- Command check: `git status --short --branch` confirmed the round branch and scoped worktree state.
- Command check: inspected the local `getOptionsLoop` hunk against `origin/codex/hls-graph-runtime-engine:ghcide/session-loader/Development/IDE/Session.hs`; the barrier/read ordering now matches the target branch.
- Command check: `git diff --check` passed.
- Command check: `ghcup run --ghc 9.12.2 -- cabal build` passed.

### Notes
Suspected cause for this round is the `getOptionsLoop` pending-barrier ordering around `waitForSessionLoaderPendingBarrier` and `S.readQueue`. No direct compile fallout occurred, so no compile-only adaptation was needed. I did not touch `HscEnvEq`, `GetModSummary`, ParentTC, plugins, benchmark configuration, `state.json`, branch history, or unrelated code.
