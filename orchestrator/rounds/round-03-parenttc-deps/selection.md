### Selected Extraction
- Milestone: Parent typecheck dependency parity
- Milestone id: milestone-002-parent-typecheck-deps
- Direction id: direction-002-restore-parenttc-fingerprint-rule
- Extracted item id: parenttc-fingerprint-rule-parity
- Extracted item summary: Restore `FileStore.typecheckParentsAction` to use the separate reverse-dependency fingerprint rule, `GetModuleGraphTransReverseDepsFingerprints`, instead of depending directly on the full `GetModuleGraph`, with only direct ParentTC dependency-path compile fallout if needed.
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-001

### Boundaries
- In scope: Parent typecheck module-graph dependency breadth after edit events; `FileStore.typecheckParentsAction`; the reverse-dependency fingerprint rule path needed to match `origin/codex/hls-graph-runtime-engine`; practical local validation for touched Haskell code; commit and push preparation for benchmark feedback.
- Out of scope: Session-loader pending-barrier behavior, `GetModSummary` or `HscEnvEq` session-option fingerprint shape, semantic-token rule behavior, HLint or lower-priority plugin parity, benchmark dependency modernization, unrelated CI cleanup, broad runtime redesign, and any branch-history cleanup.
- Concurrent batch context: none; the active roadmap places this family in `lane-serial-benchmark-parity`, and this extraction follows the completed session-loader benchmark feedback before any later suspected cause is selected.

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
Milestone 2 is the next dependency-ready milestone in the active roadmap. Its only milestone dependency, `milestone-001-session-loader-boundary`, is marked done after round `round-02-session-loader-boundary` merged as `7b8a0075c1f73aa1cc9d4d087d84810d15a6dda2`; benchmark workflow `25646814655` succeeded for that commit, but operator feedback says the matrix still differs. The roadmap therefore makes `direction-002-restore-parenttc-fingerprint-rule` ready as the next serial one-cause experiment, while later `GetModSummary`, plugin-rule, and residual-triage milestones still depend on feedback from this ParentTC round.
