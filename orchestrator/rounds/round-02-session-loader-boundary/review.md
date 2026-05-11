### Checks Run
- Command: `git status --short --branch`
  Result: pass. Output showed `## orchestrator/round-02-session-loader-boundary`, one modified production file `ghcide/session-loader/Development/IDE/Session.hs`, and untracked round artifacts under `orchestrator/rounds/round-02-session-loader-boundary/`.
- Command: `git diff --name-only && git ls-files --others --exclude-standard`
  Result: pass. Tracked production diff is limited to `ghcide/session-loader/Development/IDE/Session.hs`; untracked files are the round artifacts `implementation-notes.md`, `plan.md`, and `selection.md`.
- Command: `git diff -- ghcide/session-loader/Development/IDE/Session.hs`
  Result: pass. Diff is a single `getOptionsLoop` ordering hunk moving `waitForSessionLoaderPendingBarrier pendingBarrier sessionState` before `S.readQueue (pendingFiles sessionState)`.
- Command: `git diff --check`
  Result: pass. No whitespace or patch hygiene output.
- Command: `diff -u <(git show origin/codex/hls-graph-runtime-engine:ghcide/session-loader/Development/IDE/Session.hs | sed -n '760,772p') <(sed -n '763,775p' ghcide/session-loader/Development/IDE/Session.hs)`
  Result: pass. No diff output; the touched `getOptionsLoop` excerpt now matches `origin/codex/hls-graph-runtime-engine` ordering exactly.
- Command: `rg -n "waitForProgressDone|WaitForShakeQueue|setSessionLoaderPendingBarrier|getOptionsLoop|waitForSessionLoaderPendingBarrier" ghcide/session-loader ghcide/src/Development/IDE/Core ghcide/src/Development/IDE/Types`
  Result: pass. The session-loader-specific code path is confined to `Session.hs`: barrier setter/waiter definitions, queue startup, and `getOptionsLoop`. `WaitForShakeQueue` is implemented in the test plugin outside this round's diff.
- Command: `rg -n "waitForProgressDone|WaitForShakeQueue"`
  Result: pass. Confirmed benchmark/test wait paths reference `waitForProgressDone` and `WaitForShakeQueue`, while this round changes only the session-loader pending-barrier ordering that can affect when those waits observe work.
- Command: `ghcup run --ghc 9.12.2 -- cabal build`
  Result: pass. Command completed successfully with `Up to date`.

### Plan Compliance
- Confirm branch and pre-review status: met. `git status --short --branch` showed the active branch `orchestrator/round-02-session-loader-boundary`.
- Compare local `getOptionsLoop` hunk against `origin/codex/hls-graph-runtime-engine`: met. The corrected excerpt comparison produced no diff.
- Edit only `ghcide/session-loader/Development/IDE/Session.hs` ordering: met. Production diff contains only the selected four-line ordering change in `getOptionsLoop`.
- Keep out-of-scope areas untouched: met. No diffs in `HscEnvEq`, `GetModSummary`, ParentTC dependencies, plugin rules, benchmark dependencies, branch history, or `orchestrator/state.json`.
- Compile-only fallout handling: met. `ghcup run --ghc 9.12.2 -- cabal build` passed, so no compile-only adaptation was required.
- Benchmark parity claim discipline: met. Local build success is recorded only as a build gate; benchmark parity remains dependent on pushed GitHub Actions artifacts.

### Decision
**APPROVED**

### Evidence
The integrated round diff is limited to one production file plus round artifacts. In `ghcide/session-loader/Development/IDE/Session.hs`, `getOptionsLoop` now obtains `pendingBarrier` and `isTestMode`, waits via `waitForSessionLoaderPendingBarrier pendingBarrier sessionState` in test mode, and only then dequeues with `S.readQueue (pendingFiles sessionState)`. The touched excerpt is identical to `origin/codex/hls-graph-runtime-engine` for the reviewed hunk.

The task-specific code-path inspection confirms `setSessionLoaderPendingBarrier`, `waitForSessionLoaderPendingBarrier`, and `getOptionsLoop` are in the session-loader path, while `waitForProgressDone` and `WaitForShakeQueue` are benchmark/test wait surfaces that can observe the changed session-loader boundary. No benchmark package configuration was touched, so the GHC 9.14.1 benchmark dry-run baseline is not applicable for this round.
