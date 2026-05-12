### Source Round
- Round id: round-05-semantic-token-rule
- Merged commit: e74533dfc2058ec435e3cc8f2f8bc5fe82fd8c75
- Evidence: `orchestrator/rounds/round-05-semantic-token-rule/selection.md` selected `milestone-004-plugin-rule-behavior` / `direction-004-semantic-token-rule-parity`; `implementation-notes.md` records the scoped restoration of semantic-token rule behavior and passing `git diff --check`, target-branch comparison, focused semantic-token library build, semantic-token plugin tests, and full `ghcup run --ghc 9.12.2 -- cabal build`; `review.md` approved the two-file semantic-token restoration; `merge.md` recorded merge readiness. Operator/controller feedback reports benchmark workflow `25678825121` (`https://github.com/soulomoon/haskell-language-server/actions/runs/25678825121`) for commit `e74533dfc2058ec435e3cc8f2f8bc5fe82fd8c75`, with relevant successful jobs `75387089613` (`bench_example (9.12, ubuntu-latest, 3.16, cabal)`), `75387089620` (`bench_example (9.14, ubuntu-latest, 3.16, cabal)`), `75387089770` (`bench_example (9.12, ubuntu-latest, 3.16, lsp-types)`), and `75387089576` (`bench_example (9.14, ubuntu-latest, 3.16, lsp-types)`), while the benchmark matrix is still different after round 05.

### Roadmap Change
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Prior revision: rev-003
- Proposed revision: rev-004
- Files changed: `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-004/roadmap.md`; `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-004/verification.md`; `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-004/retry-subloop.md`; `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/roadmap-history.md`; `orchestrator/roadmap-updates/round-05-semantic-token-rule-roadmap-update.md`

### Rationale
The merged round satisfies direction 004's selected behavior target: the current branch restored ordinary semantic-token rule behavior and the related logging surface to match `origin/codex/hls-graph-runtime-engine` for the scoped plugin-rule suspect. The post-push benchmark workflow completed successfully across the relevant cabal and lsp-types jobs, but operator feedback says the matrix is still different, so semantic-token rule parity should be recorded as completed evidence rather than final parity. Because rev-003 is already a used active revision and the latest feedback changes future sequencing, this update preserves rev-003 immutability by proposing rev-004. Milestone 4 remains pending/ready while plugin-rule suspects remain, with direction 005 HLint and lower-priority plugin parity as the next dependency-ready candidate.

### State Activation
- Requires state.json roadmap metadata update: yes
- New roadmap_dir when applicable: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-004
