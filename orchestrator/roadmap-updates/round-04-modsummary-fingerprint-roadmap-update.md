### Source Round
- Round id: round-04-modsummary-fingerprint
- Merged commit: 7edc43b2511131e05c170a6937090902a42362f6
- Evidence: `orchestrator/rounds/round-04-modsummary-fingerprint/selection.md` selected `milestone-003-modsummary-fingerprint` / `direction-003-restore-modsummary-fingerprint-shape`; `implementation-notes.md` records the scoped removal of option-hash fingerprint plumbing and passing `git diff --check`, exact option-hash symbol scan, target-branch comparison, and `ghcup run --ghc 9.12.2 -- cabal build`; `review.md` approved the four-file `GetModSummary` fingerprint restoration; `merge.md` recorded merge readiness. Operator/controller feedback reports Benchmark workflow `25669605154` (`https://github.com/soulomoon/haskell-language-server/actions/runs/25669605154`) for commit `7edc43b2511131e05c170a6937090902a42362f6`, with relevant successful jobs `75353293267` (`bench_example (9.12, ubuntu-latest, 3.16, cabal)`), `75353293332` (`bench_example (9.14, ubuntu-latest, 3.16, cabal)`), `75353293329` (`bench_example (9.12, ubuntu-latest, 3.16, lsp-types)`), and `75353293306` (`bench_example (9.14, ubuntu-latest, 3.16, lsp-types)`), while the benchmark matrix is still different after round 04.

### Roadmap Change
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Prior revision: rev-002
- Proposed revision: rev-003
- Files changed: `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-003/roadmap.md`; `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-003/verification.md`; `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-003/retry-subloop.md`; `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/roadmap-history.md`; `orchestrator/roadmap-updates/round-04-modsummary-fingerprint-roadmap-update.md`

### Rationale
The merged round satisfies Milestone 3's completion condition for the selected code behavior: the current branch no longer carries the separate option-hash value through `HscEnvEq`, session-loader component info, or `getModSummaryFromImports`, and the `GetModSummary` rule call matches the target-branch fingerprint shape for this suspected cause. The post-push benchmark workflow succeeded across the relevant cabal and lsp-types jobs, but operator feedback says the matrix is still different, so Milestone 3 should be marked done as evidence rather than final parity. Because rev-002 is already a used active revision and the latest feedback changes milestone status and future sequencing, this update preserves rev-002 immutability by proposing rev-003. Milestone 4 remains pending and is now dependency-ready as the next serial candidate, starting with semantic-token rule parity under `direction-004-semantic-token-rule-parity`.

### State Activation
- Requires state.json roadmap metadata update: yes
- New roadmap_dir when applicable: orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-003
