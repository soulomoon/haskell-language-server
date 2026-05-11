### Selected Extraction
- Milestone: `GetModSummary` fingerprint parity
- Milestone id: milestone-003-modsummary-fingerprint
- Direction id: direction-003-restore-modsummary-fingerprint-shape
- Extracted item id: modsummary-fingerprint-shape-parity
- Extracted item summary: Restore `GetModSummary`, `HscEnvEq`, and session-loader option hash plumbing to match `origin/codex/hls-graph-runtime-engine` fingerprint shape, with only direct compile fallout.
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Roadmap revision: rev-002
- Roadmap dir: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-002

### Boundaries
- In scope: Session option hash plumbing and the connected `GetModSummary`/`HscEnvEq` fingerprint behavior named by Milestone 3 and direction 003, plus compile-only adaptations that are necessary to preserve the target-branch behavior on this branch.
- Out of scope: Semantic-token rule behavior, HLint or lower-priority plugin diffs, residual broad diff triage, benchmark CI cleanup, runtime redesign, test weakening, unrelated refactors, upstream PR work, and any production change beyond this single suspected cause.
- Concurrent batch context: None. The roadmap's serial benchmark-parity lane and `max_parallel_rounds=1` require this extraction to run alone after Milestone 2 feedback.

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
Milestones 1 and 2 are marked done in rev-002, and Milestone 3 depends only on `milestone-002-parent-typecheck-deps`. Direction 003's precondition is satisfied by Benchmark workflow `25665471061` on commit `8baa5fed64cc0aa8504c5e0c50014cff8b736d33`, including successful job `75340567668`, plus operator feedback that the benchmark still differs. Because the roadmap requires serial one-cause benchmark parity experiments, the next dependency-ready extraction is Milestone 3 / direction 003, limited to the `hscOptionHash` and `GetModSummary` fingerprint-shape difference before any plugin-rule or residual-triage work.
