#!/usr/bin/env python3
"""Convert one HLS benchmark matrix result into a versioned JSON artifact."""

from __future__ import annotations

import argparse
import csv
import json
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


REPOSITORY = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
COMMIT = re.compile(r"^[0-9a-f]{40}$")
MEMORY = re.compile(r"^(-?[0-9]+(?:\.[0-9]+)?)MB$")

FIELD_NAMES = {
    "Version": "version",
    "Configuration": "configuration",
    "name": "benchmark",
    "success": "success",
    "samples": "samples",
    "startup": "startup_ms",
    "setup": "setup_ms",
    "userT": "user_time_ms",
    "delayedT": "delayed_time_ms",
    "1stBuildT": "first_build_ms",
    "avgPerRespT": "average_response_ms",
    "totalT": "total_time_ms",
    "rulesBuilt": "rules_built",
    "rulesChanged": "rules_changed",
    "rulesVisited": "rules_visited",
    "rulesTotal": "rules_total",
    "ruleEdges": "rule_edges",
    "ghcRebuilds": "ghc_rebuilds",
    "maxResidency": "max_residency_mb",
    "allocatedBytes": "allocated_mb",
}

INTEGER_FIELDS = {
    "samples",
    "rules_built",
    "rules_changed",
    "rules_visited",
    "rules_total",
    "rule_edges",
    "ghc_rebuilds",
}
FLOAT_FIELDS = {
    "startup_ms",
    "setup_ms",
    "user_time_ms",
    "delayed_time_ms",
    "first_build_ms",
    "average_response_ms",
    "total_time_ms",
}
MEMORY_FIELDS = {"max_residency_mb", "allocated_mb"}


def parse_timestamp(value: str) -> str:
    parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    if parsed.tzinfo is None:
        raise ValueError("timestamp must include a UTC offset")
    return parsed.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


def parse_value(field: str, value: str) -> Any:
    if field == "success":
        if value not in {"True", "False"}:
            raise ValueError(f"invalid success value: {value!r}")
        return value == "True"
    if field in INTEGER_FIELDS:
        return int(value)
    if field in FLOAT_FIELDS:
        return float(value)
    if field in MEMORY_FIELDS:
        match = MEMORY.fullmatch(value)
        if match is None:
            raise ValueError(f"invalid memory value for {field}: {value!r}")
        return float(match.group(1))
    return value


def read_results(path: Path) -> list[dict[str, Any]]:
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle, skipinitialspace=True)
        headers = [header.strip() for header in (reader.fieldnames or [])]
        missing = sorted(set(FIELD_NAMES) - set(headers))
        if missing:
            raise ValueError(f"benchmark CSV is missing columns: {', '.join(missing)}")

        results: list[dict[str, Any]] = []
        for raw_row in reader:
            row = {(key or "").strip(): (value or "").strip() for key, value in raw_row.items()}
            converted = {
                FIELD_NAMES[source]: parse_value(FIELD_NAMES[source], row[source])
                for source in FIELD_NAMES
            }
            if converted["version"] != "HEAD":
                raise ValueError(f"observer result must describe HEAD, got {converted['version']!r}")
            results.append(converted)

    if not results:
        raise ValueError("benchmark CSV contains no result rows")
    return results


def repository(value: str) -> str:
    if REPOSITORY.fullmatch(value) is None:
        raise argparse.ArgumentTypeError("repository must have OWNER/REPO form")
    return value


def commit(value: str) -> str:
    value = value.lower()
    if COMMIT.fullmatch(value) is None:
        raise argparse.ArgumentTypeError("commit must be a full 40-character SHA")
    return value


def arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--results", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--upstream-repository", required=True, type=repository)
    parser.add_argument("--upstream-ref", required=True)
    parser.add_argument("--upstream-commit", required=True, type=commit)
    parser.add_argument("--workflow-repository", required=True, type=repository)
    parser.add_argument("--run-id", required=True, type=int)
    parser.add_argument("--run-attempt", required=True, type=int)
    parser.add_argument("--artifact-name", required=True)
    parser.add_argument("--ghc", required=True)
    parser.add_argument("--example", required=True)
    parser.add_argument("--runner-os", required=True)
    parser.add_argument("--runner-arch", required=True)
    parser.add_argument("--runner-image", default="unknown")
    parser.add_argument("--runner-image-version", default="unknown")
    parser.add_argument(
        "--measured-at",
        default=datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
    )
    return parser.parse_args()


def main() -> None:
    args = arguments()
    results = read_results(args.results)
    successful = sum(result["success"] for result in results)
    workflow_url = f"https://github.com/{args.workflow_repository}/actions/runs/{args.run_id}"
    commit_url = f"https://github.com/{args.upstream_repository}/commit/{args.upstream_commit}"

    document = {
        "schema_version": 1,
        "upstream": {
            "repository": args.upstream_repository,
            "ref": args.upstream_ref,
            "commit": args.upstream_commit,
            "commit_url": commit_url,
        },
        "workflow": {
            "repository": args.workflow_repository,
            "run_id": args.run_id,
            "run_attempt": args.run_attempt,
            "run_url": workflow_url,
            "artifact_name": args.artifact_name,
        },
        "measurement": {
            "timestamp": parse_timestamp(args.measured_at),
            "ghc": args.ghc,
            "example": args.example,
            "runner": {
                "os": args.runner_os,
                "arch": args.runner_arch,
                "image": args.runner_image,
                "image_version": args.runner_image_version,
            },
        },
        "summary": {
            "successful_cases": successful,
            "total_cases": len(results),
        },
        "results": results,
    }

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
