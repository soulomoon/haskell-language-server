# Roadmap

## Status Legend

- `pending`
- `in-progress`
- `done`

## Goal

Make the current branch's benchmark behavior converge with
`origin/codex/hls-graph-runtime-engine` by aligning one plausible behavioral
diff at a time, committing and pushing after each update, then stopping for
GitHub benchmark feedback.

## Alignment Summary

- Approved thesis: the branch still differs from
  `origin/codex/hls-graph-runtime-engine`; the safest way to identify the
  causal performance difference is a serial experiment where each round restores
  one suspected code area to target-branch behavior.
- Success criteria: after a pushed round, the relevant GitHub benchmark matrix
  no longer shows the targeted difference, or the round cleanly eliminates that
  suspected cause and leaves the next suspect ready for extraction.
- Non-goals: do not batch several suspects in one update, do not redesign the
  runtime, do not weaken tests, do not clean unrelated benchmark CI or branch
  history, and do not open or update upstream PRs as part of this roadmap.
- Chosen strategy: serial one-cause patches. Each round changes only the
  current suspected cause, validates enough to avoid obvious breakage, commits,
  pushes, and stops for operator review of the benchmark matrix.
- Latest feedback: pushed commit
  `8baa5fed64cc0aa8504c5e0c50014cff8b736d33` restored the ParentTC
  reverse-dependency fingerprint rule and Benchmark workflow `25665471061`
  reached a successful `bench_example (9.12, ubuntu-latest, 3.16, cabal)` job
  (`75340567668`), but operator feedback says the benchmark is still
  different. Treat Milestone 2 as complete evidence, not final parity.
- Deferred alternatives: bulk-restoring every differing runtime file, adding
  temporary benchmark instrumentation before parity attempts, or reverting the
  larger branch wholesale.

## Outcome Boundaries

- In scope: session-loader progress/test barrier behavior, parent typecheck
  module-graph dependency shape, `GetModSummary`/`HscEnvEq` fingerprint
  behavior, semantic-token rule behavior, and lower-priority plugin/runtime
  compatibility diffs only if earlier rounds do not resolve the matrix.
- Out of scope: benchmark dependency modernization, formatting-only benchmark
  output changes, unrelated plugin refactors, release packaging, and upstream
  review fixes unrelated to benchmark parity.
- Repo-wide invariants: see `orchestrator/project-contract.md`; this roadmap
  records only the serial benchmark-parity strategy.
- Completed history: keep compact notes in
  `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/roadmap-history.md`
  instead of copying completed item bodies into new active revisions.

## Global Sequencing Rules

- Run milestones serially unless the operator explicitly changes the strategy.
- Each extracted implementation round must restore exactly one suspected area
  to `origin/codex/hls-graph-runtime-engine` behavior, plus direct compile
  fallout only.
- After each update: run local checks that are practical for the touched files,
  commit, push the current branch, and stop. Do not continue into the next
  suspected cause before benchmark feedback is available.
- If a round fails to build, repair only the current round's targeted area and
  record the build failure evidence.
- Treat GitHub Actions benchmark artifacts as the acceptance signal for
  performance parity; local inspection can justify the next experiment but does
  not prove final parity.

## Parallel Lanes

- `lane-serial-benchmark-parity`: all candidate directions in this revision
  are serial by default because benchmark feedback is the dependency between
  rounds.

## Milestones

### [done] Milestone 1: Session-loader progress boundary parity
Milestone id: milestone-001-session-loader-boundary
Depends on:
Intent: Align the test/session-loader ordering that decides whether work is
counted under `userT` or `delayedT`.
Completion signal: A pushed round restores the session-loader barrier behavior
to match `origin/codex/hls-graph-runtime-engine`, and benchmark feedback shows
whether the `edit` `userT`/`delayedT` split converges.
Parallel lane: lane-serial-benchmark-parity
Coordination notes: This is the first extraction because the linked job showed
matching dirty-key counts for `edit` while `waitForProgressDone` returned early
on HEAD and `WaitForShakeQueue` absorbed the real work.
Completion notes: Round `round-02-session-loader-boundary` merged as
`7b8a0075c1f73aa1cc9d4d087d84810d15a6dda2`, restoring the selected
`getOptionsLoop` pending-barrier ordering. Benchmark workflow
`25646814655` succeeded for that commit, including job `75278189663`, but
operator feedback says the benchmark remains different; continue to Milestone 2.

Candidate directions:
- Direction id: direction-001-restore-pending-barrier-order
  Summary: Completed by round `round-02-session-loader-boundary`; detailed
    evidence is recorded in roadmap history and the round artifacts.

### [done] Milestone 2: Parent typecheck dependency parity
Milestone id: milestone-002-parent-typecheck-deps
Depends on: milestone-001-session-loader-boundary
Intent: Align the dependency breadth of parent typecheck work after edit events.
Completion signal: A pushed round restores `typecheckParentsAction` behavior to
target-branch use of `GetModuleGraphTransReverseDepsFingerprints`, and
benchmark feedback shows whether rule counts or delayed work converge further.
Parallel lane: lane-serial-benchmark-parity
Coordination notes: Start this only if milestone 1 does not fully explain the
remaining matrix difference.
Completion notes: Round `round-03-parenttc-deps` merged as
`8baa5fed64cc0aa8504c5e0c50014cff8b736d33`, restoring
`typecheckParentsAction` to use
`GetModuleGraphTransReverseDepsFingerprints` as the separate fingerprint rule
over `GetModuleGraph`. Benchmark workflow `25665471061` had successful job
`75340567668`, but operator feedback says the benchmark is still different;
continue to Milestone 3.

Candidate directions:
- Direction id: direction-002-restore-parenttc-fingerprint-rule
  Summary: Completed by round `round-03-parenttc-deps`; detailed evidence is
    recorded in roadmap history and the round artifacts.

### [pending] Milestone 3: `GetModSummary` fingerprint parity
Milestone id: milestone-003-modsummary-fingerprint
Depends on: milestone-002-parent-typecheck-deps
Intent: Align session option hashing and `GetModSummary` newness behavior with
the target branch.
Completion signal: A pushed round removes or adapts the current branch's
`hscOptionHash` path to match target-branch behavior, and benchmark feedback
shows whether HLint, semantic-token, or rule-count differences converge.
Parallel lane: lane-serial-benchmark-parity
Coordination notes: This touches several connected files and should not be
extracted until narrower candidates have been measured.
Readiness notes: Ready as the next serial candidate after benchmark workflow
`25665471061` on commit `8baa5fed64cc0aa8504c5e0c50014cff8b736d33` produced
successful job `75340567668`, while operator feedback says the benchmark is
still different.

Candidate directions:
- Direction id: direction-003-restore-modsummary-fingerprint-shape
  Summary: Restore `GetModSummary`, `HscEnvEq`, and session-loader option hash
    plumbing to the target branch's fingerprint shape.
  Why it matters now: The current branch includes `hscOptionHash` in
    `GetModSummary` fingerprints while the target branch does not.
  Preconditions: benchmark feedback from milestone 2 is available; satisfied by
    workflow `25665471061` plus operator feedback that differences remain.
  Parallel hints: Serial only.
  Boundary notes: Keep changes limited to session option hash plumbing and
    direct compile fallout.
  Extraction notes: Record any retained compile-only GHC compatibility
    adaptation explicitly in implementation notes.

### [pending] Milestone 4: Plugin rule behavior parity
Milestone id: milestone-004-plugin-rule-behavior
Depends on: milestone-003-modsummary-fingerprint
Intent: Align plugin rules that can change benchmark rule counts or stale-value
reuse behavior.
Completion signal: Pushed rounds restore remaining plugin-rule behavior to
target branch where relevant, and benchmark feedback either converges or rules
out those diffs.
Parallel lane: lane-serial-benchmark-parity
Coordination notes: Split plugin areas into separate rounds when they touch
different benchmark surfaces.

Candidate directions:
- Direction id: direction-004-semantic-token-rule-parity
  Summary: Restore semantic-token rule behavior to target branch if earlier
    milestones leave semantic-token or hover/code-action rule-count diffs.
  Why it matters now: Current branch changes semantic tokens from ordinary
    `define` behavior to early-cutoff old-value behavior gated by file of
    interest.
  Preconditions: benchmark feedback from milestone 3 is available.
  Parallel hints: Serial only.
  Boundary notes: Do not combine with HLint or eval-plugin changes.
  Extraction notes: Verify any changed semantic-token behavior with focused
    plugin or build checks before push.

- Direction id: direction-005-hlint-and-lower-priority-plugin-parity
  Summary: Restore HLint masking and any remaining plugin diffs only if logs or
    benchmark feedback point at them.
  Why it matters now: These diffs are lower-probability causes but remain
    branch differences.
  Preconditions: semantic-token behavior has either converged or been ruled
    out.
  Parallel hints: Serial only.
  Boundary notes: Keep each plugin family separate unless the operator approves
    batching.
  Extraction notes: Prefer the smallest one-file parity restoration first.

### [pending] Milestone 5: Residual parity triage
Milestone id: milestone-005-residual-triage
Depends on: milestone-004-plugin-rule-behavior
Intent: Re-evaluate remaining codebase diffs only after higher-probability
runtime and plugin causes are exhausted.
Completion signal: The roadmap either reaches benchmark parity or produces a
short residual-diff report with evidence that remaining differences are
non-causal or require a new roadmap family.
Parallel lane: lane-serial-benchmark-parity
Coordination notes: This milestone should not become a broad cleanup pass.

Candidate directions:
- Direction id: direction-006-residual-diff-review
  Summary: Compare remaining branch diffs against benchmark artifacts and
    propose the next single-cause experiment or a stop condition.
  Why it matters now: After targeted parity attempts, remaining differences
    need fresh evidence rather than assumption.
  Preconditions: milestone 4 complete or explicitly skipped by operator.
  Parallel hints: Serial only.
  Boundary notes: Read-only by default unless the guider extracts a new
    one-cause implementation round.
  Extraction notes: Produce a concise cause ranking and do not modify code
    unless a new implementation round is selected.
