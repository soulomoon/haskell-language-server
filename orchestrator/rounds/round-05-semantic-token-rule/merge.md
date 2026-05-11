### Squash Commit
- Title: fix: restore semantic token rule parity
- Summary: Restores the semantic-token rule and logging surface to match `origin/codex/hls-graph-runtime-engine` for the approved Milestone 4 suspect. The round is limited to `plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Internal.hs` and `plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Types.hs`, and review confirmed both scoped files match the target branch exactly with no compile-only adaptation remaining.

### Merge Readiness
- Base branch freshness: confirmed. The round branch `orchestrator/round-05-semantic-token-rule` and base branch `improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` both resolve to `fc42d981b`.
- Merge ordering satisfied: yes. The review decision is approved, completed rounds 02-04 are the only declared predecessors, `max_parallel_rounds=1` is respected, and `selection.md` declares no `depends_on_round_ids`, no `merge_after_item_ids`, and no parallel group.
- Pending dependencies: none.

### Follow-Up Notes
After squash merge, commit and push the current branch, then stop for operator benchmark feedback. Do not start HLint, eval-plugin, residual triage, or benchmark CI cleanup until the benchmark matrix shows whether this serial one-suspect semantic-token experiment converged.
