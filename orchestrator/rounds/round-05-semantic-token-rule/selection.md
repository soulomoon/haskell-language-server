### Selected Extraction
- Milestone: Plugin rule behavior parity
- Milestone id: milestone-004-plugin-rule-behavior
- Direction id: direction-004-semantic-token-rule-parity
- Extracted item id: semantic-token-rule-parity
- Extracted item summary: Restore semantic-token rule behavior to match `origin/codex/hls-graph-runtime-engine`, with only direct compile fallout.
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Roadmap revision: rev-003
- Roadmap dir: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-003

### Boundaries
- In scope: Semantic-token rule behavior named by Milestone 4 / direction 004, including target-branch parity for ordinary rule definition versus early-cutoff old-value behavior gated by file-of-interest state, plus direct compile-only adaptations required by that restoration.
- Out of scope: HLint masking, lower-priority plugin diffs, eval-plugin changes, residual broad diff triage, benchmark CI cleanup, runtime redesign, test weakening, unrelated refactors, upstream PR work, and any production change beyond this single suspected cause.
- Concurrent batch context: None. The roadmap's serial benchmark-parity lane and `max_parallel_rounds=1` require this extraction to run alone after Milestone 3 feedback.

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
Milestones 1 through 3 are marked done in rev-003, and Milestone 4 depends only on `milestone-003-modsummary-fingerprint`. Direction 004's precondition is satisfied by benchmark workflow `25669605154` on commit `7edc43b2511131e05c170a6937090902a42362f6`, plus operator feedback that the benchmark matrix is still different after round 04. Because direction 005 requires semantic-token behavior to have converged or been ruled out, the first dependency-ready candidate is Milestone 4 / direction 004, limited to the semantic-token rule parity experiment before any HLint, lower-priority plugin, or residual triage work.
