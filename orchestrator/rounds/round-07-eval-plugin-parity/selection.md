### Selected Extraction
- Milestone: Plugin rule behavior parity
- Milestone id: milestone-004-plugin-rule-behavior
- Direction id: direction-006-eval-plugin-command-parity
- Extracted item id: eval-plugin-command-handler-parity
- Extracted item summary: Restore eval-plugin command handler parity in `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` to match `origin/codex/hls-graph-runtime-engine`, with only direct compile fallout allowed if later roles find any.
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Roadmap revision: rev-005
- Roadmap dir: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-005

### Boundaries
- In scope: `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` parity against `origin/codex/hls-graph-runtime-engine`, plus direct compile fallout only if required by that restoration.
- Out of scope: class-plugin CPP parity, residual triage, runtime/session changes, benchmark CI changes, unrelated plugin refactors, and any broad cleanup.
- Concurrent batch context: none; this roadmap revision keeps plugin-rule parity work in `lane-serial-benchmark-parity`.

### Scheduler Fields
```json
{
  "depends_on_round_ids": [],
  "merge_after_item_ids": [],
  "parallel_group": null,
  "merge_ready": false
}
```

### Rationale
The active state is `rev-005` for roadmap `2026-05-11-00-hls-benchmark-parity`, with no active or pending rounds and `max_parallel_rounds` set to 1. Milestones 1 through 3 are complete, and Milestone 4 is dependency-ready after `round-05-semantic-token-rule` and `round-06-hlint-plugin-parity` both produced pushed benchmark evidence without final parity.

Direction 006 is ready because the rev-005 roadmap records benchmark workflow `25732089803` still differing after HLint settings-mask parity, while `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs` now has no diff against `origin/codex/hls-graph-runtime-engine`. A narrow comparison still shows `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` differs from the target branch, and the same benchmark run includes current eval execute-command evidence for `lsp-types / 9.14`: upstream succeeded while HEAD timed out after `kick/done`. The previous workflow `25678825121` had that eval scenario passing on HEAD, so this selection treats eval-plugin parity as the next serial experiment, not as deterministic proof of the remaining performance cause.

Direction 007 is not selected because its precondition requires direction 006 to complete and benchmark feedback to still show differences. Milestone 5 is not selected because Milestone 4 remains open.
