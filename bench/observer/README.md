# Upstream benchmark observer tooling

`upstream-benchmark-observer.yml` turns this fork into a stateless benchmark runner. It checks out an exact commit from `haskell/haskell-language-server`, rewrites the temporary benchmark config to measure only that `HEAD`, runs the Cabal and lsp-types workloads under GHC 9.12 and 9.14, and uploads one normalized JSON artifact per matrix coordinate.

If `observer_repository` is supplied to `workflow_dispatch`, the final job sends `benchmark-complete` only after all four artifacts have uploaded successfully. The dispatch payload includes the exact upstream SHA and workflow run identity. Configure `OBSERVER_TOKEN` with **Contents: write** access to the observer repository to enable that callback.

Local tooling tests:

```bash
python3 -m unittest discover -s bench/observer -p 'test_*.py' -v
```
