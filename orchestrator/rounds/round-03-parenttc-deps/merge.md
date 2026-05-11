### Squash Commit
- Title: fix: restore ParentTC reverse dependency fingerprint rule
- Summary: Restores `Development.IDE.Core.FileStore.typecheckParentsAction` to compute ParentTC reverse dependencies through `GetModuleGraphTransReverseDepsFingerprints` as a separate fingerprint rule over `GetModuleGraph`. The approved round keeps the production change limited to `ghcide/src/Development/IDE/Core/FileStore.hs`, matching the selected behavior from `origin/codex/hls-graph-runtime-engine` without batching other benchmark-parity suspects.

### Merge Readiness
- Base branch freshness: confirmed. The round worktree is on `orchestrator/round-03-parenttc-deps` at `6d682697453f8b15e552979e10760eaa03858cb6`, the local base branch `improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` is at the same commit, and fetched `origin/improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` (`7b8a0075c1f73aa1cc9d4d087d84810d15a6dda2`) is an ancestor of the local base.
- Merge ordering satisfied: yes. `selection.md` declares `depends_on_round_ids: []` and `merge_after_item_ids: []`; `review.md` and `review-record.json` mark the round approved.
- Pending dependencies: none.

### Follow-Up Notes
After squash merge, stop for operator benchmark feedback before selecting later suspected causes from the roadmap. The review evidence says local validation passed with `git diff --check` and `ghcup run --ghc 9.12.2 -- cabal build`, but benchmark parity remains a post-push/operator-feedback decision.
