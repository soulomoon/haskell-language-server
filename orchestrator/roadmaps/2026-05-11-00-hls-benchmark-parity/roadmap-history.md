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
- `round-04-modsummary-fingerprint`: merged as
  `7edc43b2511131e05c170a6937090902a42362f6` with title
  `fix: restore GetModSummary fingerprint parity`. The round removed the
  selected option-hash fingerprint plumbing from `GetModSummary`, `HscEnvEq`,
  and session-loader component info paths, matching
  `origin/codex/hls-graph-runtime-engine` for this suspected cause while leaving
  unrelated branch adaptations intact. Round evidence recorded
  `git diff --check`, the exact option-hash symbol scan, target-branch
  comparison, and `ghcup run --ghc 9.12.2 -- cabal build` passing. Benchmark
  workflow `25669605154`
  (`https://github.com/soulomoon/haskell-language-server/actions/runs/25669605154`)
  succeeded for relevant jobs: cabal 9.12 `75353293267`, cabal 9.14
  `75353293332`, lsp-types 9.12 `75353293329`, and lsp-types 9.14
  `75353293306`. Operator feedback says the benchmark matrix is still
  different, so the `GetModSummary` candidate is ruled out as a full explanation
  and Milestone 4 remains the next serial candidate.
- `round-05-semantic-token-rule`: merged as
  `e74533dfc2058ec435e3cc8f2f8bc5fe82fd8c75` with title
  `fix: restore semantic token rule parity`. The round restored the
  semantic-token rule and logging surface in
  `plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs`
  and
  `plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`
  to match `origin/codex/hls-graph-runtime-engine` for the selected
  plugin-rule suspect. Round evidence recorded `git diff --check`, exact
  target-branch comparison for the two scoped files, the focused semantic-token
  library build, all 30 semantic-token plugin tests passing, and
  `ghcup run --ghc 9.12.2 -- cabal build` passing. Benchmark workflow
  `25678825121`
  (`https://github.com/soulomoon/haskell-language-server/actions/runs/25678825121`)
  completed successfully for relevant jobs: cabal 9.12 `75387089613`, cabal
  9.14 `75387089620`, lsp-types 9.12 `75387089770`, and lsp-types 9.14
  `75387089576`. Operator feedback says the benchmark matrix is still
  different, so semantic-token rule parity is complete evidence but not final
  parity, and Milestone 4 remains open for the next plugin-rule candidate.
- `round-06-hlint-plugin-parity`: merged as
  `0891e18eb7171d1c5de26cf86e02d198e339c360` with title
  `fix: restore HLint settings mask parity`. The round restored
  `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs` to target-branch parity
  for `GetHlintSettings` by calling `argsSettings flags` directly instead of
  under `uninterruptibleMask_`. Round evidence recorded `git diff --check`,
  exact target-branch comparison for the scoped HLint file, focused
  `hls-hlint-plugin` build under GHC 9.12.2, and full
  `ghcup run --ghc 9.12.2 -- cabal build` passing. Benchmark workflow
  `25732089803`
  (`https://github.com/soulomoon/haskell-language-server/actions/runs/25732089803`)
  completed successfully for all benchmark jobs, but operator feedback and
  downloaded artifacts say the benchmark matrix is still different. Current
  evidence includes cabal 9.12 completions/code-actions/hover rule-count
  shifts, cabal 9.14 documentSymbols and eval code-lens shifts, lsp-types 9.12
  hover/getDefinition/code-actions shifts, and lsp-types 9.14 eval execute
  single-line code lens failing on HEAD after `kick/done`. The previous run
  `25678825121` had that eval scenario passing on HEAD, so record it as current
  benchmark evidence rather than deterministic proof. HLint settings-mask
  parity is complete evidence but not final parity, and Milestone 4 remains
  open for eval-plugin parity next.

## Superseded Revisions

- `rev-001`: superseded by `rev-002` after `round-03-parenttc-deps` completed
  Milestone 2 and benchmark feedback kept the serial parity search moving to
  Milestone 3.
- `rev-002`: superseded by `rev-003` after
  `round-04-modsummary-fingerprint` completed Milestone 3 and benchmark feedback
  kept the serial parity search moving to Milestone 4.
- `rev-003`: superseded by `rev-004` after
  `round-05-semantic-token-rule` completed direction 004 as evidence and
  benchmark feedback kept Milestone 4 open for direction 005.
- `rev-004`: superseded by `rev-005` after
  `round-06-hlint-plugin-parity` completed the HLint settings-mask portion of
  direction 005 as evidence and benchmark feedback kept Milestone 4 open for
  the next lower-priority plugin candidate.
