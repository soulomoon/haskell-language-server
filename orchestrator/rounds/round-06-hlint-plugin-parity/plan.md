### Goal
Restore the selected HLint settings-rule behavior in `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs` to match `origin/codex/hls-graph-runtime-engine`, limited to the `GetHlintSettings` / `argsSettings` suspected cause plus direct compile fallout only.

### Approach
Use the target branch as the source of truth for this one file. The current branch differs from `origin/codex/hls-graph-runtime-engine` by one HLint line: this branch wraps `argsSettings flags` in `uninterruptibleMask_`, while the target branch calls `argsSettings flags` directly. Implement the exact target-branch behavior in `Ide.Plugin.Hlint` and avoid class-plugin, eval-plugin, residual-diff, benchmark-CI, or runtime changes.

Keep the implementation sequential. Worker fan-out is not justified because the selected scope is one source file and one rule behavior, and splitting it would create coordination overhead without independent ownership boundaries.

### Steps
1. Confirm the starting state with `git status --short --branch` and note any unrelated local artifacts before editing.
2. Recheck the scoped target comparison:
   `git diff --color=never origin/codex/hls-graph-runtime-engine -- plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`.
3. In `plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`, change only the `GetHlintSettings` rule body from `liftIO $ uninterruptibleMask_ $ argsSettings flags` to `liftIO $ argsSettings flags`.
4. Leave imports and surrounding rule structure unchanged unless a focused compile error directly caused by this restoration requires a minimal HLint-only adjustment. The target branch currently keeps the existing `Control.Exception` import, so do not remove it preemptively.
5. Confirm the scoped file now matches the target branch with:
   `git diff --exit-code origin/codex/hls-graph-runtime-engine -- plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`.
6. If a verification command fails, repair only direct compile fallout from this HLint restoration. Do not widen into `hls-class-plugin`, `hls-eval-plugin`, residual triage, or unrelated cleanup.
7. Record implementation notes with the exact diff evidence and command outcomes. After review and merge handling, the controller/merger should commit, push, and stop for benchmark feedback before any next suspected cause.

### Verification
Run these checks after implementation:

1. `git status --short --branch`
2. `git diff --check`
3. `git diff --exit-code origin/codex/hls-graph-runtime-engine -- plugins/hls-hlint-plugin/src/Ide/Plugin/Hlint.hs`
4. `ghcup run --ghc 9.12.2 -- cabal build haskell-language-server:hls-hlint-plugin`
5. `ghcup run --ghc 9.12.2 -- cabal build`

If the focused HLint build is unavailable for an environmental reason, record the exact failure and still run the full GHC 9.12.2 build unless that same environment blocker prevents it. No benchmark-parity claim should be made from local checks alone; the pushed benchmark workflow remains the acceptance signal for whether this suspected cause is ruled in or out.
