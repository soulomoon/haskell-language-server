# Roadmap

Roadmap revision:
`orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-005/`.

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
  `0891e18eb7171d1c5de26cf86e02d198e339c360` restored HLint settings-mask
  behavior. Benchmark workflow `25732089803`
  (`https://github.com/soulomoon/haskell-language-server/actions/runs/25732089803`)
  completed successfully for all benchmark jobs, but operator feedback and
  downloaded artifacts show the benchmark matrix still differs from
  `codex/hls-graph-runtime-engine`. Treat HLint settings-mask parity as
  complete evidence, not final parity, and continue to the next
  dependency-ready plugin-rule candidate.
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
Completion notes: Completed by `round-02-session-loader-boundary`; see roadmap
history and round artifacts. Benchmark feedback kept the matrix different, so
the serial search continued to Milestone 2.

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
Completion notes: Completed by `round-03-parenttc-deps`; see roadmap history
and round artifacts. Benchmark feedback kept the matrix different, so the
serial search continued to Milestone 3.

Candidate directions:
- Direction id: direction-002-restore-parenttc-fingerprint-rule
  Summary: Completed by round `round-03-parenttc-deps`; detailed evidence is
    recorded in roadmap history and the round artifacts.

### [done] Milestone 3: `GetModSummary` fingerprint parity
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
Completion notes: Completed by `round-04-modsummary-fingerprint`; see roadmap
history and round artifacts. Benchmark workflow `25669605154` succeeded for the
relevant matrix jobs, but operator feedback says the benchmark is still
different, so the serial search continues to Milestone 4.

Candidate directions:
- Direction id: direction-003-restore-modsummary-fingerprint-shape
  Summary: Completed by round `round-04-modsummary-fingerprint`; detailed
    evidence is recorded in roadmap history and the round artifacts.

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
Readiness notes: Ready to continue inside this milestone after benchmark
workflow `25732089803` on commit
`0891e18eb7171d1c5de26cf86e02d198e339c360` succeeded for all benchmark jobs,
while operator feedback and artifact inspection say the benchmark matrix is
still different. Directions 004 and 005 are complete evidence; the remaining
plugin-family diffs against `origin/codex/hls-graph-runtime-engine` are
`plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` and
`plugins/hls-class-plugin/src/Ide/Plugin/Class/Types.hs`. Direction 006 is the
next dependency-ready candidate because the current artifacts include an eval
execute-command failure on `lsp-types / 9.14`, with a determinism caveat from
the previous benchmark run.

Candidate directions:
- Direction id: direction-004-semantic-token-rule-parity
  Summary: Completed by round `round-05-semantic-token-rule`; detailed
    evidence is recorded in roadmap history and the round artifacts. Benchmark
    feedback kept the matrix different, so this candidate is ruled out as a
    full explanation.

- Direction id: direction-005-hlint-and-lower-priority-plugin-parity
  Summary: Completed first as HLint settings-mask parity by
    `round-06-hlint-plugin-parity`; detailed evidence is recorded in roadmap
    history and the round artifacts. Benchmark feedback kept the matrix
    different, so HLint settings masking is ruled out as a full explanation.
  Why it matters now: HLint was the smallest one-file lower-priority plugin
    restoration and is now complete evidence rather than remaining work.
  Preconditions: semantic-token behavior has been ruled out as a full
    explanation by workflow `25678825121` plus operator feedback that
    differences remain.
  Parallel hints: Serial only.
  Boundary notes: Do not fold eval-plugin or class-plugin follow-up work into
    the completed HLint round.
  Extraction notes: Completed extraction `hlint-settings-mask-parity`.

- Direction id: direction-006-eval-plugin-command-parity
  Summary: Restore the remaining eval-plugin `Handlers.hs` diff to
    target-branch parity as the next lower-priority plugin candidate.
  Why it matters now: Benchmark workflow `25732089803` still differs after
    HLint parity, and its `lsp-types / 9.14` matrix shows `eval execute
    single-line code lens` with upstream `success=True` and HEAD
    `success=False`, timing out waiting for the `workspace/executeCommand`
    response after `kick/done`. Previous workflow `25678825121` had this
    scenario passing on HEAD, so treat the failure as current benchmark
    evidence but not proof of a deterministic eval regression.
  Preconditions: HLint settings-mask parity has been ruled out as a full
    explanation by workflow `25732089803`, and the scoped HLint file now
    matches `origin/codex/hls-graph-runtime-engine`.
  Parallel hints: Serial only.
  Boundary notes: Touch only
    `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` plus direct
    compile fallout if required. Do not batch the class-plugin CPP diff,
    runtime files, benchmark CI, or residual broad triage.
  Extraction notes: Compare the eval-plugin file against
    `origin/codex/hls-graph-runtime-engine`, restore only the selected
    target-branch shape, run a focused eval-plugin build if practical, then the
    usual branch validation before push.

- Direction id: direction-007-class-plugin-ghc-cpp-parity
  Summary: Restore the remaining class-plugin GHC-version CPP shape only if
    the eval-plugin candidate is ruled out by benchmark feedback.
  Why it matters now: This is the other remaining plugin-family diff, but the
    current benchmark artifacts point more directly at eval execute-command
    behavior.
  Preconditions: Direction 006 completed and benchmark feedback still shows
    differences.
  Parallel hints: Serial only.
  Boundary notes: Touch only
    `plugins/hls-class-plugin/src/Ide/Plugin/Class/Types.hs` plus direct
    compile fallout if required.
  Extraction notes: Keep as the next plugin fallback, not the immediate
    selection while eval evidence is fresher.

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
- Direction id: direction-008-residual-diff-review
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
