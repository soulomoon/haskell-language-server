# Project Contract

This file records repo-wide invariants shared by every roadmap family and
round. Keep roadmap revisions focused on current coordination; point here for
stable contracts instead of restating them in every role or roadmap file.

## Stable Interfaces

- Event schemas: LSP benchmark output is observed through `ghcide-bench` logs,
  especially progress notifications, `WaitForShakeQueue`, build-key counts,
  and `bench-results/unprofiled/*/results.csv`.
- Golden logs and fixtures: GitHub Actions benchmark artifacts from
  `soulomoon/haskell-language-server` are the reviewer-visible comparison
  evidence for this roadmap family.
- Dry-run or command-rendering output: benchmark target compatibility may be
  checked with Cabal dry-runs, but performance parity is decided by the
  uploaded benchmark result matrix.
- Package and module boundaries: runtime/session changes are expected mostly
  in `ghcide/session-loader`, `ghcide/src/Development/IDE/Core`,
  `ghcide/src/Development/IDE/Types`, `hls-graph`, and narrowly relevant
  plugin modules.
- Public compatibility facades: current branch behavior should match
  `origin/codex/hls-graph-runtime-engine` in each targeted area unless a
  compile-only adaptation is explicitly recorded.

## Alignment Invariants

- Human-approved architecture constraints: restore one suspected behavioral
  difference at a time to match `origin/codex/hls-graph-runtime-engine`; do not
  batch multiple suspects into one benchmark experiment.
- Compatibility promises: after each update, commit and push the current branch
  and stop so the operator can inspect whether the GitHub benchmark matrix now
  matches the target branch.
- Explicit non-goals that should not be reopened without a new roadmap family:
  broad runtime redesign, unrelated benchmark CI cleanup, upstream PR creation,
  branch-history cleanup, and deleting or weakening tests to force parity.

## Verification Anchors

- Invariants every reviewer should consider when touched: each round must name
  the exact suspected cause, show the diff against
  `origin/codex/hls-graph-runtime-engine`, and keep the change scoped to that
  cause plus direct compile fallout.
- Baseline commands that protect shared contracts: `git diff --check`, a
  targeted Cabal build when Haskell code changes, and post-push inspection of
  the GitHub benchmark workflow/artifacts for the current branch.

## Update Rule

Update this file only when the repo-wide invariant itself changes. When a
roadmap temporarily narrows or extends an invariant, record the override in the
active roadmap bundle and keep the durable rule here.
