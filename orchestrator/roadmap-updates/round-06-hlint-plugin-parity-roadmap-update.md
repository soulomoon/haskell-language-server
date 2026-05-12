### Source Round
- Round id: round-06-hlint-plugin-parity
- Merged commit: `0891e18eb7171d1c5de26cf86e02d198e339c360`
- Evidence: `orchestrator/rounds/round-06-hlint-plugin-parity/selection.md` selected `hlint-settings-mask-parity` from `milestone-004-plugin-rule-behavior`; `implementation-notes.md` records restoring `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs` to direct `argsSettings flags`; `review.md` and `review-record.json` approve the scoped HLint parity change; `merge.md` records the squash title `fix: restore HLint settings mask parity`. Benchmark workflow `25732089803` (`https://github.com/soulomoon/haskell-language-server/actions/runs/25732089803`) succeeded for all benchmark jobs, but downloaded artifacts still differ from `codex/hls-graph-runtime-engine`.

### Roadmap Change
- Roadmap id: 2026-05-11-00-hls-benchmark-parity
- Prior revision: rev-004
- Proposed revision: rev-005
- Files changed: `orchestrator/roadmap-updates/round-06-hlint-plugin-parity-roadmap-update.md`, `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-005/roadmap.md`, `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-005/verification.md`, `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-005/retry-subloop.md`, `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/roadmap-history.md`

### Rationale
Round 06 completed the selected HLint settings-mask parity experiment and the scoped HLint file now matches `origin/codex/hls-graph-runtime-engine`, but benchmark workflow `25732089803` did not converge the matrix. The new revision keeps rev-004 immutable, records HLint settings masking as completed evidence rather than final parity, and keeps Milestone 4 open for remaining plugin-family suspects.

The latest artifacts still show structural differences after the pushed head `0891e18eb7171d1c5de26cf86e02d198e339c360`: cabal 9.12 completions `rulesBuilt` `3166 -> 955`, code actions `1561 -> 2825`, hover `3256 -> 1948`; cabal 9.14 documentSymbols after edit `rulesBuilt` `31 -> 53` and `rulesVisited` `1178 -> 815`, with eval code lens lower `ghcRebuilds` `197 -> 93`; lsp-types 9.12 hover `8199 -> 6293`, getDefinition `9416 -> 7942`, code actions `9631 -> 9235`; and lsp-types 9.14 eval execute single-line code lens upstream success `True` versus HEAD success `False`, timing out waiting for the `workspace/executeCommand` response after `kick/done`.

The next dependency-ready candidate encoded in rev-005 is `direction-006-eval-plugin-command-parity`, extracting one serial round for `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs`. This is preferred over the remaining class-plugin CPP diff because current benchmark evidence points at eval execute-command behavior, while preserving the caveat that previous workflow `25678825121` had the same scenario passing on HEAD.

### State Activation
- Requires state.json roadmap metadata update: yes
- New roadmap_dir when applicable: `orchestrator/roadmaps/2026-05-11-00-hls-benchmark-parity/rev-005`
