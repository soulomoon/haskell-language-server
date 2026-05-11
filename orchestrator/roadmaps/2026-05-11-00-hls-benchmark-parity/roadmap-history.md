# Roadmap History

Roadmap family: `2026-05-11-00-hls-benchmark-parity`

Keep compact completed-history notes here when a roadmap revision is superseded
or a round materially changes future coordination. Active roadmap revisions
should describe live and future work, not copy every completed item forward.

## Completed Rounds

- `round-02-session-loader-boundary`: merged as
  `7b8a0075c1f73aa1cc9d4d087d84810d15a6dda2` with title
  `fix: restore session-loader pending barrier order`. The round changed only
  `ghcide/session-loader/Development/IDE/Session.hs` to restore
  `getOptionsLoop` pending-barrier ordering so test mode waits with
  `waitForSessionLoaderPendingBarrier` before dequeuing pending files with
  `S.readQueue`, matching `origin/codex/hls-graph-runtime-engine` for the
  selected hunk. Round evidence recorded `git diff --check` and
  `ghcup run --ghc 9.12.2 -- cabal build` passing. Benchmark workflow
  `25646814655` succeeded on the pushed commit, including job `75278189663`,
  but operator feedback says the benchmark matrix is still different, so the
  session-loader candidate is ruled out as a full explanation and Milestone 2
  remains the next serial candidate.

## Superseded Revisions

- none yet
