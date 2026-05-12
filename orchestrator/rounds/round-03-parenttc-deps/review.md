### Checks Run
- Command: `git status --short --branch`
  Result: pass. Worktree is on `orchestrator/round-03-parenttc-deps` with one modified production file, `ghcide/src/Development/IDE/Core/FileStore.hs`, plus untracked round artifacts under `orchestrator/rounds/round-03-parenttc-deps/`.
- Command: `git diff --name-status`
  Result: pass. Tracked production diff is limited to `M ghcide/src/Development/IDE/Core/FileStore.hs`.
- Command: `find orchestrator/rounds/round-03-parenttc-deps -maxdepth 2 -type f -print`
  Result: pass. Round artifact files present before review were `implementation-notes.md`, `selection.md`, and `plan.md`.
- Command: `git diff -- ghcide/src/Development/IDE/Core/FileStore.hs`
  Result: pass. The local production diff is a single expression change in `typecheckParentsAction`, replacing `useNoFile_ GetModuleGraph` with `useWithSeparateFingerprintRule_ GetModuleGraphTransReverseDepsFingerprints GetModuleGraph nfp`.
- Command: `git diff origin/codex/hls-graph-runtime-engine -- ghcide/src/Development/IDE/Core/FileStore.hs`
  Result: pass. The selected ParentTC expression matches target-branch behavior. Remaining file differences versus the target branch are unrelated export/import/style and `typecheckParents` binding-shape differences that this round intentionally left out of scope.
- Command: `git show origin/codex/hls-graph-runtime-engine:ghcide/src/Development/IDE/Core/FileStore.hs | rg -n -C 8 "typecheckParentsAction|useWithSeparateFingerprintRule_|GetModuleGraphTransReverseDepsFingerprints|useNoFile_ GetModuleGraph"`
  Result: pass. Target branch uses `transitiveReverseDependencies nfp <$> useWithSeparateFingerprintRule_ GetModuleGraphTransReverseDepsFingerprints GetModuleGraph nfp` in `typecheckParentsAction`.
- Command: `git diff --check`
  Result: pass. No whitespace or patch hygiene errors were reported.
- Command: `ghcup run --ghc 9.12.2 -- cabal build`
  Result: pass. Cabal reported `Up to date` under GHC 9.12.2 after refreshing ghcup metadata.
- Command: `find . orchestrator -path '*bench-results*' -o -name 'results.csv' -o -name '*artifact*' | head -80`
  Result: pass with limitation. No local benchmark results or GitHub artifact extracts were present, only `./scripts/release/download-gh-artifacts.sh`.
- Command: `find /Volumes/src/soulomoonHLS.worktrees/improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties -path '*bench-results*' -o -name 'results.csv' | head -80`
  Result: pass with limitation. No upstream artifact CSVs were available locally, so the ParentTC `GetBuildKeysBuilt`, `GetBuildKeysChanged`, and `GetBuildKeysVisited` artifact comparison cannot be performed in this review.

### Plan Compliance
- Step 1, confirm branch and understandable worktree state: met. `git status --short --branch` shows `orchestrator/round-03-parenttc-deps` with only `FileStore.hs` modified and round artifacts untracked.
- Step 2, update `typecheckParentsAction` to use `useWithSeparateFingerprintRule_ GetModuleGraphTransReverseDepsFingerprints GetModuleGraph nfp`: met. The production diff contains exactly that expression replacement.
- Step 3, compare the touched hunk against `origin/codex/hls-graph-runtime-engine` and avoid unrelated target-branch changes: met. The touched expression matches the target branch; unrelated target-branch differences in the same file were not imported.
- Step 4, fix only direct compile fallout if present and avoid unrelated runtime/session/plugin/benchmark areas: met. No compile fallout appeared in the GHC 9.12.2 Cabal build, and no other production files are modified.
- Step 5, record implementation notes and stop for benchmark feedback: met for local review scope. `implementation-notes.md` records the parity comparison and validation. Commit/push and post-push benchmark inspection were not performed because the operator instructions for this review explicitly say not to commit or push; no local benchmark artifacts were available to compare.

### Decision
**APPROVED**

### Evidence
The integrated round result restores the selected ParentTC dependency behavior only. `ghcide/src/Development/IDE/Core/FileStore.hs` now computes `revs` with:

```haskell
useWithSeparateFingerprintRule_
  GetModuleGraphTransReverseDepsFingerprints
  GetModuleGraph
  nfp
```

This matches the target branch expression in `origin/codex/hls-graph-runtime-engine`. The local diff does not batch other suspected benchmark causes, does not touch session-loader behavior, `GetModSummary`, `HscEnvEq`, semantic-token rules, HLint/plugin behavior, benchmark configuration, or unrelated runtime internals. Required local baseline checks passed: `git status --short --branch`, `git diff --check`, and `ghcup run --ghc 9.12.2 -- cabal build`.
