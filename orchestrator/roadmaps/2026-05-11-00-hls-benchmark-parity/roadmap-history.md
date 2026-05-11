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
- `round-03-parenttc-deps`: merged as
  `8baa5fed64cc0aa8504c5e0c50014cff8b736d33` with title
  `fix: restore ParentTC reverse dependency fingerprint rule`. The round
  changed only `ghcide/src/Development/IDE/Core/FileStore.hs` to restore
  `typecheckParentsAction` to use
  `GetModuleGraphTransReverseDepsFingerprints` as a separate fingerprint rule
  over `GetModuleGraph`, matching the selected target-branch behavior in
  `origin/codex/hls-graph-runtime-engine`. Round evidence recorded
  `git diff --check` and `ghcup run --ghc 9.12.2 -- cabal build` passing.
  Benchmark workflow `25665471061` reached successful job `75340567668` for
  commit `8baa5fed64cc0aa8504c5e0c50014cff8b736d33`, but operator feedback
  says the benchmark is still different, so the ParentTC dependency candidate is
  ruled out as a full explanation and Milestone 3 remains the next serial
  candidate.

## Superseded Revisions

- `rev-001`: superseded by `rev-002` after `round-03-parenttc-deps` completed
  Milestone 2 and benchmark feedback kept the serial parity search moving to
  Milestone 3.
