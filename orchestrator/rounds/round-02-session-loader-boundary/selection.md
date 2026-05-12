### Selected Extraction
- Milestone: Session-loader progress boundary parity
- Milestone id: milestone-001-session-loader-boundary
- Direction id: direction-001-restore-pending-barrier-order
- Extracted item id: session-loader-boundary-parity
- Extracted item summary: Restore `getOptionsLoop` pending-barrier ordering in `ghcide/session-loader/Development/IDE/Session.hs` to match `origin/codex/hls-graph-runtime-engine`, with only direct compile fallout if needed.
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-001

### Boundaries
- In scope: Session-loader progress/test barrier behavior around `waitForProgressDone`, `WaitForShakeQueue`, `setSessionLoaderPendingBarrier`, and `getOptionsLoop`; local validation for the touched area; commit and push preparation for benchmark feedback.
- Out of scope: `HscEnvEq`, `GetModSummary`, ParentTC dependency shape, semantic-token rules, HLint or other plugin-rule parity, benchmark dependency modernization, unrelated CI cleanup, broad runtime redesign, and any unrelated branch-history work.
- Concurrent batch context: none; the active roadmap places all candidate directions in `lane-serial-benchmark-parity`, and benchmark feedback from this round gates later milestones.

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
This is the first dependency-ready milestone in the active roadmap: it has no milestone dependencies, matches the controller's requested `session-loader-boundary-parity` extraction, and is explicitly ordered before ParentTC, `GetModSummary`, and plugin-rule parity work. The roadmap identifies this direction as the only current diff with a direct explanation for the observed `edit` attribution shift where dirty-key counts matched but `waitForProgressDone` returned early and `WaitForShakeQueue` absorbed the real work. Running it now preserves the approved serial one-cause benchmark strategy and gives the operator a single pushed experiment to compare against `origin/codex/hls-graph-runtime-engine`.
