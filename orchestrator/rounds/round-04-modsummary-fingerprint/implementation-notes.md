### Changes Made
- ghcide/src/Development/IDE/Core/Rules.hs: restored `GetModSummary` to read only `hscEnv` from `GhcSession` and call `getModSummaryFromImports session fp ...`, removing the current-branch `hscOptionHash` argument from the rule path.
- ghcide/src/Development/IDE/Core/Compile.hs: restored `getModSummaryFromImports` to the target-branch signature and removed the separate `hscOptionHash` component from `computeFingerprint`; file path, imports, module location, and preprocessor option strings still participate in the fingerprint.
- ghcide/src/Development/IDE/Types/HscEnvEq.hs: removed `hscOptionHash` from the export list, record fields, constructor arguments, invariant comment, and `NFData` pattern shape so `HscEnvEq` no longer carries the option hash.
- ghcide/session-loader/Development/IDE/Session/Ghc.hs: removed `ComponentInfo.componentOptionHash`, stopped passing an option hash into `newHscEnvEq`, removed construction of the field in `addComponentInfo`, and deleted the now-unused `getOptionHash` helper.

### Tests
- Command: `git status --short --branch`
  Result: confirmed active branch `orchestrator/round-04-modsummary-fingerprint`; final worktree contains only the four scoped Haskell files plus this round's untracked notes directory.
- Command: `rg -n "hscOptionHash|componentOptionHash|getOptionHash" ghcide/session-loader/Development/IDE/Session/Ghc.hs ghcide/src/Development/IDE/Types/HscEnvEq.hs ghcide/src/Development/IDE/Core/Compile.hs ghcide/src/Development/IDE/Core/Rules.hs`
  Result: no matches.
- Command: `git diff origin/codex/hls-graph-runtime-engine -- ghcide/session-loader/Development/IDE/Session/Ghc.hs ghcide/src/Development/IDE/Types/HscEnvEq.hs ghcide/src/Development/IDE/Core/Compile.hs ghcide/src/Development/IDE/Core/Rules.hs`
  Result: target comparison shows no remaining option-hash fingerprint/HscEnvEq differences; remaining differences are pre-existing local branch changes in `Session/Ghc.hs`, `Compile.hs`, and `Rules.hs`.
- Command: `git diff --check`
  Result: passed.
- Command: `ghcup run --ghc 9.12.2 -- cabal build`
  Result: passed.

### Notes
No compile-fallout edits outside the owned write scope were required. The target-branch comparison still shows unrelated pre-existing local differences in touched files, including `mask_` in `emptyHscEnv`, GHC-version import reshaping in `Compile.hs`, and existing `Rules.hs` session/loading/debug/comment changes; these were left intact per round boundaries. No commit, push, merge, or `state.json` edit was performed.
