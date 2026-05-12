### Squash Commit
- Title: fix: restore HLint settings mask parity
- Summary: Restores the HLint `GetHlintSettings` behavior to target-branch parity by calling `argsSettings flags` directly in `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`. The approved round is limited to the selected HLint settings-rule masking suspect, leaves unrelated plugin and runtime surfaces untouched, and review confirmed the scoped HLint file matches `origin/codex/hls-graph-runtime-engine`.

### Merge Readiness
- Base branch freshness: confirmed locally. The round branch `orchestrator/round-06-hlint-plugin-parity` and base branch `improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` both resolve to `9a27b96c94b0d53e1c64e82288f5c8aac548658b`, and `git rev-list --left-right --count improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties...orchestrator/round-06-hlint-plugin-parity` reports `0	0`.
- Merge ordering satisfied: yes. `review.md` and `review-record.json` approve the round, `orchestrator/state.json` has no active rounds or pending merge rounds, `last_completed_round` is `round-05-semantic-token-rule`, `max_parallel_rounds=1` is respected, and `selection.md` declares no `depends_on_round_ids`, no `merge_after_item_ids`, and no parallel group.
- Pending dependencies: none.

### Follow-Up Notes
After squash merge, commit and push the current branch, then stop for operator benchmark feedback. Do not continue into class-plugin, eval-plugin, residual diff triage, benchmark CI cleanup, or another suspected cause until the benchmark matrix shows whether this serial HLint settings-mask experiment converged or was ruled out.
