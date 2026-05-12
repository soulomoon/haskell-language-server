### Goal
Restore `GetModSummary`, `HscEnvEq`, and session-loader option-hash plumbing to match the `origin/codex/hls-graph-runtime-engine` fingerprint shape for the single suspected benchmark difference: the current branch includes `hscOptionHash` in `GetModSummary` fingerprints, while the target branch does not.

### Approach
Keep this as a sequential, single-cause parity restoration. The implementation should remove the option-hash value from the `HscEnvEq` data path and from `getModSummaryFromImports`, then fix only direct compile fallout from that removal. Shared roadmap and repo invariants remain in `orchestrator/project-contract.md`: match the targeted behavior from `origin/codex/hls-graph-runtime-engine`, avoid batching other suspected benchmark causes, commit and push after verification, then stop for operator benchmark feedback.

The expected target-branch shape is:

```haskell
getModSummaryFromImports session fp ...
```

and the `computeFingerprint` input list should include file path, imports, module location, and preprocessor option strings, but not a separate `hscOptionHash` component.

### Steps
1. Confirm the round starts on `orchestrator/round-04-modsummary-fingerprint` with an understandable worktree state:
   `git status --short --branch`.
2. In `ghcide/src/Development/IDE/Core/Rules.hs`, restore the `GetModSummary` rule call shape to match the target branch: use `hscEnv <$> use_ GhcSession f` and call `getModSummaryFromImports session fp ...` without extracting or passing `hscOptionHash`.
3. In `ghcide/src/Development/IDE/Core/Compile.hs`, restore `getModSummaryFromImports` to the target-branch signature by removing the `String` option-hash parameter, and remove `Util.fingerprintString hscOptionHash` from `computeFingerprint`. Keep the existing GHC-version adaptations and preprocessor `opts` fingerprinting intact.
4. In `ghcide/src/Development/IDE/Types/HscEnvEq.hs`, remove the `hscOptionHash` export, field, constructor argument, invariant comment, and `NFData` arity adjustment so `HscEnvEq` again matches the target branch shape.
5. In `ghcide/session-loader/Development/IDE/Session/Ghc.hs`, remove the `ComponentInfo.componentOptionHash` field, stop passing it to `newHscEnvEq`, remove its construction in `addComponentInfo`, and delete `getOptionHash` if no remaining code uses it. Do not alter `getCacheDirs`, `rawComponentHash`, cache directory behavior, or the existing `mask_` compile-fallout change unless the build proves a direct dependency.
6. Compare each touched file against `origin/codex/hls-graph-runtime-engine` and record any intentional local adaptation. The intended remaining differences should be only unrelated pre-existing branch changes or direct compile fallout; do not pull in semantic-token, HLint, plugin-rule, benchmark workflow, or broader runtime changes.
7. Record implementation notes with the exact target-branch comparison, local validation commands, pushed branch, and SHA. After commit and push, stop for operator benchmark feedback rather than selecting another suspected cause.

### Verification
Run the roadmap baseline checks for a Haskell source change:

```bash
git status --short --branch
git diff --check
ghcup run --ghc 9.12.2 -- cabal build
```

Also perform the `GetModSummary` fingerprint alignment checks before review:

```bash
rg -n "hscOptionHash|componentOptionHash|getOptionHash" ghcide/session-loader/Development/IDE/Session/Ghc.hs ghcide/src/Development/IDE/Types/HscEnvEq.hs ghcide/src/Development/IDE/Core/Compile.hs ghcide/src/Development/IDE/Core/Rules.hs
git diff origin/codex/hls-graph-runtime-engine -- ghcide/session-loader/Development/IDE/Session/Ghc.hs ghcide/src/Development/IDE/Types/HscEnvEq.hs ghcide/src/Development/IDE/Core/Compile.hs ghcide/src/Development/IDE/Core/Rules.hs
```

The reviewer should confirm that the final diff removes only the selected option-hash fingerprint plumbing plus explicitly documented direct compile fallout. Post-push benchmark parity is decided from the GitHub Actions benchmark artifacts for the branch, not from local reasoning alone.
