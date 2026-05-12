### Squash Commit
- Title: fix: restore eval plugin handler parity
- Summary: Restores `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` to match `origin/codex/hls-graph-runtime-engine` for the selected eval-plugin command handler parity experiment. The scoped eval handler diff against the target branch is empty, no direct compile fallout edits were needed, and the round intentionally stops before class-plugin parity or residual benchmark triage.

### Merge Readiness
- Base branch freshness: confirmed locally. The round branch `orchestrator/round-07-eval-plugin-parity` is at `a3de68f546b81cc675a67c18e9eb455d37b07829`, the local base branch `improve-hls-runtime-keep-async-only-databse-keys-downsweep-skip-non-dirties` is at `721c4f232b9da7519db34173478c04dbc941ab7e`, and the base tip is an ancestor of the round branch.
- Merge ordering satisfied: yes. The selection declares no `depends_on_round_ids` and no `merge_after_item_ids`; `orchestrator/state.json` has no active rounds and no pending merge rounds; the roadmap marks direction 006 as the next serial plugin-rule candidate after round 06.
- Pending dependencies: none observable locally. `review.md` and `review-record.json` approve the round.

### Follow-Up Notes
The focused eval test command `ghcup run --ghc 9.12.2 -- cabal test hls-eval-plugin-tests` failed non-blockingly with 66 of 68 tests passing. The two failures were `eval.Property checking` and `eval.Property checking with exception`, both producing the existing QuickCheck dependency diagnostic (`Add QuickCheck to your cabal dependencies to run this test.`) instead of the expected property result. The focused `hls-eval-plugin` build, `git diff --check`, scoped target-branch diff check, and full `ghcup run --ghc 9.12.2 -- cabal build` passed, so this is recorded as non-blocking local test-environment behavior for this scoped parity merge.
