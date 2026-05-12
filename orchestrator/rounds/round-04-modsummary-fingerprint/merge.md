### Squash Commit
- Title: fix: restore GetModSummary fingerprint parity
- Summary: This round restores the selected GetModSummary fingerprint shape to match `origin/codex/hls-graph-runtime-engine` for the single Milestone 3 suspected cause. It removes the option-hash plumbing from `HscEnvEq`, the session loader, and `getModSummaryFromImports`, and restores the rule call to `getModSummaryFromImports session fp ...` without batching unrelated benchmark suspects.

### Merge Readiness
- Base branch freshness: confirmed. The fetched `origin/improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` is an ancestor of this round checkout, and the local base ref matches the current round checkout head.
- Merge ordering satisfied: yes. The review is approved, the round has no declared `merge_after_item_ids`, the serial lane has no concurrent batch, and the only external prerequisite, `round-03-parenttc-deps`, is completed per operator context.
- Pending dependencies: none.

### Follow-Up Notes
After squash merge, keep this as a single-cause benchmark parity experiment: push the base branch and wait for the GitHub Actions benchmark artifacts before selecting any next suspected cause. Performance parity is not established by local build success alone.
