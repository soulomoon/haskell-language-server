### Goal

Restore eval-plugin command handler parity for
`plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` against
`origin/codex/hls-graph-runtime-engine`, then stop after local verification so
the pushed benchmark workflow can decide whether this suspected cause is ruled
in or out.

### Approach

Keep this round as a serial, one-file parity experiment. The implementer must
compare the scoped file directly against `origin/codex/hls-graph-runtime-engine`
and apply only the selected target-branch behavior needed for eval-plugin
command handler parity. Direct compile fallout is allowed only when it is
caused by restoring that file's target behavior and must be named explicitly in
the implementation notes.

Do not batch class-plugin CPP parity, residual triage, runtime/session changes,
benchmark CI changes, broad plugin cleanup, or unrelated files into this round.
Repo-wide invariants are in `orchestrator/project-contract.md`; this plan
narrows them to the selected eval-plugin extraction.

### Steps

1. Confirm the starting state:
   `git status --short --branch`.
2. Compare the scoped file against the target branch:
   `git diff origin/codex/hls-graph-runtime-engine -- plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs`.
3. Edit only
   `plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs` to restore the
   selected `origin/codex/hls-graph-runtime-engine` behavior for eval-plugin
   command handlers.
4. If the focused build exposes direct compile fallout from that restoration,
   make the smallest required follow-up edit and record why it is direct
   fallout. Do not use compile fallout as permission to change runtime/session,
   class-plugin, benchmark, or unrelated plugin files.
5. Recompare the scoped file with the target branch and record whether any
   remaining diff is intentional compile/import-shape adaptation:
   `git diff origin/codex/hls-graph-runtime-engine -- plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs`.
6. Verify the final changed-file set is still within this round's boundary:
   `git diff --name-only`.
7. Commit and push only after local verification passes or after recording the
   exact unavailable/failing command evidence required by the orchestrator loop.
   Stop after push for benchmark feedback; do not continue to class-plugin
   parity or residual triage.

### Verification

Run these checks, in order, unless a command is unavailable; if unavailable,
record the exact command and failure reason.

1. `git status --short --branch`
2. `git diff --check`
3. `git diff --name-only`
4. `git diff origin/codex/hls-graph-runtime-engine -- plugins/hls-eval-plugin/src/Ide/Plugin/Eval/Handlers.hs`
5. `ghcup run --ghc 9.12.2 -- cabal build haskell-language-server:hls-eval-plugin`
6. `ghcup run --ghc 9.12.2 -- cabal test hls-eval-plugin-tests`
7. `ghcup run --ghc 9.12.2 -- cabal build`

The focused eval-plugin build/test commands are preferred because this is a
plugin-rule round. If the test target is impractical in the local environment,
the implementer must say why and still run the focused library build plus the
full 9.12.2 build gate.

### Worker Fan-Out

No worker fan-out. This round is intentionally sequential because the selected
scope is one source file, the roadmap lane is serial, and benchmark feedback is
the dependency between suspected causes.
