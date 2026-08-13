#!/usr/bin/env python3
"""Create a benchmark config that measures only the checked-out HEAD."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


TOP_LEVEL_KEY = re.compile(r"^[A-Za-z][A-Za-z0-9_-]*:\s*(?:#.*)?$")


def head_only_config(source: str) -> str:
    """Replace the top-level versions section without requiring PyYAML."""
    lines = source.splitlines(keepends=True)
    starts = [index for index, line in enumerate(lines) if line.rstrip() == "versions:"]
    if len(starts) != 1:
        raise ValueError(f"expected exactly one top-level versions section, found {len(starts)}")

    start = starts[0]
    end = next(
        (
            index
            for index in range(start + 1, len(lines))
            if TOP_LEVEL_KEY.fullmatch(lines[index].rstrip("\r\n"))
        ),
        None,
    )
    if end is None:
        raise ValueError("versions must be followed by another top-level config key")

    newline = "\r\n" if lines[start].endswith("\r\n") else "\n"
    return "".join(lines[:start] + [f"versions:{newline}", f"- HEAD{newline}", newline] + lines[end:])


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("source", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()

    rendered = head_only_config(args.source.read_text(encoding="utf-8"))
    args.output.write_text(rendered, encoding="utf-8")


if __name__ == "__main__":
    main()
