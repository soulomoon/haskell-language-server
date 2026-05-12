### Selected Extraction
- Milestone: Plugin rule behavior parity
- Milestone id: milestone-004-plugin-rule-behavior
- Direction id: direction-005-hlint-and-lower-priority-plugin-parity
- Extracted item id: hlint-settings-mask-parity
- Extracted item summary: Restore the HLint settings rule masking behavior in `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs` to match `origin/codex/hls-graph-runtime-engine`, with only direct compile fallout.
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Roadmap revision: rev-004
- Roadmap dir: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-004

### Boundaries
- In scope: The one-file HLint parity restoration for `GetHlintSettings` / `argsSettings` behavior in `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`, plus direct compile-only adaptation if required by that restoration.
- Out of scope: `hls-class-plugin`, `hls-eval-plugin`, any other lower-priority plugin family, residual broad diff triage, runtime redesign, benchmark CI cleanup, unrelated refactors, test weakening, upstream PR work, and any production change outside this single HLint suspected cause.
- Concurrent batch context: None. The active roadmap uses the serial benchmark-parity lane, `max_parallel_rounds=1`, and direction 005 says to keep plugin families separate unless the operator approves batching.

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
Milestone 4 is pending in rev-004, its dependency chain through `milestone-003-modsummary-fingerprint` is complete, and direction 004 has already been ruled out as a full explanation by benchmark workflow `25678825121` plus operator feedback that the matrix remains different after commit `e74533dfc2058ec435e3cc8f2f8bc5fe82fd8c75`. Direction 005 is therefore dependency-ready. The branch diff against `origin/codex/hls-graph-runtime-engine` contains a single HLint plugin file difference, while the remaining class and eval plugin diffs are separate plugin families. Selecting `hlint-settings-mask-parity` honors the roadmap's smallest-one-file-first extraction note without batching unrelated plugin surfaces.
