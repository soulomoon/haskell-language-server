# Retry Subloop Policy

Active bundle:
`orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-002/`.

This file records only repo- and roadmap-specific retry policy. Keep shared
runtime mechanics in the runtime skill references and controller state schema.

## Same-Round Retry

- Default: enabled only for the same suspected cause selected for the current
  round.
- Enabled extracted items:
  - build repair within the same targeted parity area
  - direct compile fallout caused by restoring target-branch behavior
- Enabled review outcomes:
  - local build failure caused by the selected parity update
  - reviewer rejection for scope leakage that can be corrected by narrowing the
    same round
- Worker-slice retry:
  - disabled unless `worker-plan.json` explicitly enables it for the current
    round.

## Pending-Merge Policy

- A reviewed round may pause in `pending-merge` only for branch freshness or a
  failed push.
- Performance uncertainty is not a reason to keep implementing in the same
  round; after commit and push, stop for operator benchmark feedback.
- If hosted CI finds a build failure, retry the same round only for a minimal
  repair to the current suspected cause.

## Roadmap Revision Rule

- If benchmark feedback changes future sequencing or rules out a suspected
  cause, publish a roadmap update or new revision rather than rewriting a used
  revision.

## Roadmap Overrides

- Do not widen a retry from one suspected cause into the next roadmap candidate.
