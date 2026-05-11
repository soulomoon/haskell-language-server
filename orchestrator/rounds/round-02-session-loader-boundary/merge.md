### Squash Commit
- Title: fix: restore session-loader pending barrier order
- Summary: Restores the `getOptionsLoop` test-mode pending-barrier wait before dequeuing pending files in `ghcide/session-loader/Development/IDE/Session.hs`, matching the reviewed `origin/codex/hls-graph-runtime-engine` ordering for the selected session-loader boundary experiment.

### Merge Readiness
- Base branch freshness: confirmed. Local `improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` and `origin/improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` both point to `b9b280835c838e76ac45b285622613adc08947fd`, and the round branch is based on the same commit.
- Merge ordering satisfied: yes. `selection.md` declares empty `depends_on_round_ids` and empty `merge_after_item_ids`; this round is the first dependency-ready item in the serial benchmark-parity lane.
- Pending dependencies: none.

### Follow-Up Notes
Review is approved in `review.md` and `review-record.json`. Local verification recorded by the implementer and reviewer includes `git diff --check` and `ghcup run --ghc 9.12.2 -- cabal build`; benchmark parity should still be judged from the pushed GitHub Actions benchmark artifacts after the controller performs the actual merge/publish steps.
