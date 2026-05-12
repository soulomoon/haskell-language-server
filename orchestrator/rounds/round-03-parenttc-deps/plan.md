### Goal
Restore ParentTC dependency behavior so `Development.IDE.Core.FileStore.typecheckParentsAction` matches `origin/codex/hls-graph-runtime-engine` for reverse-dependency invalidation breadth: ParentTC should use the separate `GetModuleGraphTransReverseDepsFingerprints` fingerprint rule instead of depending directly on the full `GetModuleGraph`.

### Approach
Keep this as a single-cause, sequential parity restoration. The implementation should change only the ParentTC reverse-dependency lookup path and any direct compile fallout required by that change. Shared roadmap and repo invariants remain in `orchestrator/project-contract.md`: match the targeted behavior from `origin/codex/hls-graph-runtime-engine`, avoid batching other suspected benchmark causes, commit and push after verification, then stop for operator benchmark feedback.

The expected code shape is the target-branch form:

```haskell
useWithSeparateFingerprintRule_
  GetModuleGraphTransReverseDepsFingerprints
  GetModuleGraph
  nfp
```

That result should still be passed through `transitiveReverseDependencies nfp`, preserving existing logging and `GetModIface` scheduling.

### Steps
1. Confirm the round starts on `orchestrator/round-03-parenttc-deps` with an understandable worktree state:
   `git status --short --branch`.
2. In `ghcide/src/Development/IDE/Core/FileStore.hs`, update `typecheckParentsAction` to compute `revs` with `useWithSeparateFingerprintRule_ GetModuleGraphTransReverseDepsFingerprints GetModuleGraph nfp` instead of `useNoFile_ GetModuleGraph`.
3. Compare the touched `FileStore.hs` hunk against `origin/codex/hls-graph-runtime-engine:ghcide/src/Development/IDE/Core/FileStore.hs`. Preserve only the ParentTC fingerprint-rule parity change and avoid pulling unrelated target-branch differences such as export-list or worker-thread import shape unless the local build requires them.
4. If compilation exposes a direct fallout from the ParentTC change, fix only that fallout in the narrow dependency path. Do not change session-loader behavior, `GetModSummary`, `HscEnvEq`, semantic-token rules, HLint/plugin behavior, benchmark configuration, or unrelated runtime internals in this round.
5. Record implementation notes with the exact parity comparison, local validation commands, pushed branch, and SHA. After commit and push, stop for operator benchmark feedback rather than selecting another suspected cause.

### Verification
Run the roadmap baseline checks for a Haskell source change:

```bash
git status --short --branch
git diff --check
ghcup run --ghc 9.12.2 -- cabal build
```

Also perform the ParentTC-specific alignment check before review:

```bash
git diff -- ghcide/src/Development/IDE/Core/FileStore.hs
git diff origin/codex/hls-graph-runtime-engine -- ghcide/src/Development/IDE/Core/FileStore.hs
```

The reviewer should confirm that the final diff restores only the selected `typecheckParentsAction` dependency behavior, plus any explicitly documented direct compile fallout. Post-push benchmark parity is decided from the GitHub Actions benchmark artifacts for the branch, not from local reasoning alone.
