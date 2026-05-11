### Goal
Restore the `ghcide/session-loader/Development/IDE/Session.hs` `getOptionsLoop` pending-barrier ordering to match `origin/codex/hls-graph-runtime-engine`, so this round isolates the session-loader progress boundary as the only benchmark experiment.

### Approach
Keep the change sequential and confined to the selected session-loader hunk. In `getOptionsLoop`, move the test-mode `waitForSessionLoaderPendingBarrier pendingBarrier sessionState` call back before `S.readQueue (pendingFiles sessionState)`, matching `origin/codex/hls-graph-runtime-engine` exactly for that ordering. Do not touch the other current differences in `Session.hs` unless the restored ordering directly causes a compile error, and record any such compile-only adaptation in the implementation notes.

Worker fan-out is not used: the owned code change is a single ordered hunk in one module, and parallel workers would add integration risk without a non-overlapping ownership boundary.

### Steps
1. Confirm the worktree is on `orchestrator/round-02-session-loader-boundary` and capture the pre-change status with `git status --short --branch`.
2. Compare the local `getOptionsLoop` hunk against `origin/codex/hls-graph-runtime-engine:ghcide/session-loader/Development/IDE/Session.hs` to verify the intended target ordering.
3. Edit only `ghcide/session-loader/Development/IDE/Session.hs` so `waitForSessionLoaderPendingBarrier` runs in test mode before `S.readQueue`, leaving the rest of `getOptionsLoop` and all unrelated `Session.hs` changes untouched.
4. Recheck the file diff against `origin/codex/hls-graph-runtime-engine` and confirm the session-loader hunk now matches the target branch ordering, with no changes to out-of-scope areas such as `HscEnvEq`, `GetModSummary`, ParentTC dependencies, plugin rules, benchmark dependencies, or branch history.
5. If compilation exposes direct fallout from this exact hunk, make the smallest local compile-only adaptation and document why it is required; otherwise do not broaden the edit.
6. Prepare the implementer result for commit and push after verification, then stop for operator benchmark feedback before any later roadmap candidate.

### Verification
- Run `git diff --check`.
- Run `ghcup run --ghc 9.12.2 -- cabal build` because this round changes Haskell source.
- Inspect the final diff for `ghcide/session-loader/Development/IDE/Session.hs` against `origin/codex/hls-graph-runtime-engine` and record that the `getOptionsLoop` barrier ordering matches the target branch.
- In the implementation notes, identify the suspected cause as the `getOptionsLoop` pending-barrier ordering around `waitForSessionLoaderPendingBarrier` and `S.readQueue`.
- After the implementation is committed and pushed by the appropriate role, rely on the GitHub Actions benchmark artifacts to decide whether benchmark parity improved; do not claim performance parity from local build success alone.
