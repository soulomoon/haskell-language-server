# Verification Checks

Roadmap revision:
`orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-005/`.

This file records repo- and roadmap-specific checks. Universal reviewer duties,
lineage requirements, evidence requirements, and approve/reject format live in
`orchestrator/roles/reviewer.md`. Repo-wide invariants live in
`orchestrator/project-contract.md`.

## Baseline Checks

- Command: `git status --short --branch`
  Why: confirm the round starts and finishes from an understandable branch and
  that unrelated local edits are not mixed into the parity experiment.
- Command: `git diff --check`
  Why: catch whitespace and patch hygiene issues before committing.
- Command: `ghcup run --ghc 9.12.2 -- cabal build`
  Why: full local build gate previously used successfully for this branch
  family; run when the round changes Haskell source unless unavailable or
  explicitly deferred with evidence.
- Command: `ghcup run --ghc 9.14.1 -- cabal build haskell-language-server:benchmark --dry-run`
  Why: use when benchmark target, Cabal/GHC compatibility, or benchmark package
  configuration is touched.

## Alignment Checks

- Criterion: one suspected cause per round.
  Check: reviewer confirms the diff only restores the selected candidate area
  to `origin/codex/hls-graph-runtime-engine` behavior plus direct compile
  fallout.
- Criterion: commit, push, then stop.
  Check: implementation notes and merge notes record the pushed branch/SHA and
  do not continue into the next candidate without operator benchmark feedback.
- Criterion: target-branch parity.
  Check: reviewer compares the touched files against
  `origin/codex/hls-graph-runtime-engine` and records any intentional
  adaptation.
- Criterion: benchmark feedback drives sequencing.
  Check: before selecting the next round, guider records the latest GitHub
  Actions benchmark run and the specific matrix fields that still differ.

## Task-Specific Checks

- For session-loader rounds, inspect logs or code paths involving
  `waitForProgressDone`, `WaitForShakeQueue`, `setSessionLoaderPendingBarrier`,
  and `getOptionsLoop`.
- For ParentTC rounds, compare `GetBuildKeysBuilt`, `GetBuildKeysChanged`, and
  `GetBuildKeysVisited` between HEAD and upstream artifacts when available.
- For `GetModSummary` fingerprint rounds, inspect downstream HLint,
  semantic-token, typecheck, and mod-summary key differences before selecting
  follow-up work.
- For plugin-rule rounds, include a focused build or plugin test when one is
  practical and record if no narrow local test exists.
- For eval-plugin rounds, compare
  `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` against
  `origin/codex/hls-graph-runtime-engine` and record whether the change is
  behavioral or compile/import-shape parity only. Benchmark workflow
  `25732089803` currently records an `lsp-types / 9.14` eval execute
  single-line code lens failure on HEAD, but workflow `25678825121` had that
  scenario passing, so reviewers should preserve the nondeterminism caveat
  until a follow-up benchmark confirms the signal.

## Manual Checks

- After each push, inspect the new GitHub Actions benchmark run for the current
  branch and compare against `origin/codex/hls-graph-runtime-engine`.
- For the latest linked run `25732089803`, preserve the current comparison
  context: all benchmark jobs completed successfully, but the matrix still
  differs. Structural differences include cabal 9.12 completions `rulesBuilt`
  `3166 -> 955`, code actions `1561 -> 2825`, hover `3256 -> 1948`; cabal
  9.14 documentSymbols after edit `rulesBuilt` `31 -> 53` and `rulesVisited`
  `1178 -> 815`, with eval code lens still lower `ghcRebuilds` `197 -> 93`;
  lsp-types 9.12 hover `8199 -> 6293`, getDefinition `9416 -> 7942`, code
  actions `9631 -> 9235`; and lsp-types 9.14 eval execute single-line code
  lens upstream success `True` versus HEAD success `False`.
- For the linked baseline run `25639387870`, preserve the known comparison
  context: `edit` had matching `rulesBuilt` and `rulesChanged` but shifted
  `userT`/`delayedT`; non-edit differences included hover/code-action rule
  counts, document symbols changed count, and eval single-line rebuild/visited
  count shifts.
- If GitHub artifacts are unavailable, record the exact unavailable source and
  stop rather than claiming parity from local reasoning alone.

## Roadmap Overrides

- Rounds in this roadmap are serial even when files look independent, because
  benchmark feedback is the dependency between experiments.
