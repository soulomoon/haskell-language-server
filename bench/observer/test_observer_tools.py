from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from export_result import read_results
from make_config import head_only_config


HERE = Path(__file__).parent


class MakeConfigTests(unittest.TestCase):
    def test_replaces_only_the_versions_section(self) -> None:
        source = """samples: 50
versions:
# examples
- upstream: origin/master
- HEAD

# plugin selection
configurations:
- All: []
"""
        self.assertEqual(
            head_only_config(source),
            """samples: 50
versions:
- HEAD

configurations:
- All: []
""",
        )

    def test_rejects_missing_versions(self) -> None:
        with self.assertRaisesRegex(ValueError, "exactly one"):
            head_only_config("samples: 50\nconfigurations: []\n")


class ExportResultTests(unittest.TestCase):
    CSV = """Version, Configuration, name, success, samples, startup, setup, userT, delayedT, 1stBuildT, avgPerRespT, totalT, rulesBuilt, rulesChanged, rulesVisited, rulesTotal, ruleEdges, ghcRebuilds, maxResidency, allocatedBytes
HEAD, All, hover, True, 50, 712.50, 1.00, 4.00, 2.00, 3.00, 0.50, 20.00, 1, 2, 3, 4, 5, 6, 421MB, 900MB
"""

    def test_reads_typed_result_rows(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory, "results.csv")
            path.write_text(self.CSV, encoding="utf-8")
            rows = read_results(path)

        self.assertEqual(rows[0]["benchmark"], "hover")
        self.assertEqual(rows[0]["startup_ms"], 712.5)
        self.assertEqual(rows[0]["max_residency_mb"], 421.0)
        self.assertTrue(rows[0]["success"])

    def test_cli_writes_versioned_metadata(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory, "results.csv")
            output = Path(directory, "benchmark.json")
            source.write_text(self.CSV, encoding="utf-8")
            subprocess.run(
                [
                    sys.executable,
                    str(HERE / "export_result.py"),
                    "--results",
                    str(source),
                    "--output",
                    str(output),
                    "--upstream-repository",
                    "haskell/haskell-language-server",
                    "--upstream-ref",
                    "master",
                    "--upstream-commit",
                    "a" * 40,
                    "--workflow-repository",
                    "soulomoon/haskell-language-server",
                    "--run-id",
                    "123",
                    "--run-attempt",
                    "1",
                    "--artifact-name",
                    "observer-benchmark-cabal-ghc-9.14",
                    "--ghc",
                    "9.14",
                    "--example",
                    "cabal",
                    "--runner-os",
                    "Linux",
                    "--runner-arch",
                    "X64",
                    "--measured-at",
                    "2026-08-13T04:00:00Z",
                ],
                check=True,
            )
            document = json.loads(output.read_text(encoding="utf-8"))

        self.assertEqual(document["schema_version"], 1)
        self.assertEqual(document["upstream"]["commit"], "a" * 40)
        self.assertEqual(document["summary"], {"successful_cases": 1, "total_cases": 1})


if __name__ == "__main__":
    unittest.main()
