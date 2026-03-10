#!/usr/bin/env bash
# Loop running HLS tasty tests until a Broken pipe or test failure is observed.
#
# Logs each run to test-logs/<pattern-slug>-loop-<n>.log, alternating between
# two files per pattern.
#
# Exit codes:
#   0 - Reached MAX_ITER without reproducing issues
#   1 - Issue reproduced (broken pipe or test failure)
#   2 - Setup error
#
# Environment variables:
#   MAX_ITER      - Maximum iterations before giving up (default: 1000)
#   SLEEP_SECS    - Seconds to sleep between iterations (default: 0)
#   SHOW_EVERY    - Print progress every N iterations (default: 1, <=0 disables)
#   LOG_STDERR    - Enable verbose stderr logging (default: 1)
#   DEBUG_DETECT  - Show debug output for detection (default: 0)
#   NO_BUILD_ONCE - Skip initial cabal build step if set
#
# Test selection (in order of precedence):
#   TEST_PATTERNS - Comma-separated list: 'pattern' or 'binary::pattern'
#   PATTERN_FILE  - File with one entry per line (# comments allowed)
#   Default       - 'ghcide-tests::open close'
#
# Examples:
#   ./flaky-test-loop.sh 50
#   TEST_PATTERNS='open close' ./flaky-test-loop.sh
#   TEST_PATTERNS='ghcide-tests::open close,func-test::progress' ./flaky-test-loop.sh

set -euo pipefail

# =============================================================================
# Configuration
# =============================================================================

MAX_ITER="${MAX_ITER:-}"
SLEEP_SECS="${SLEEP_SECS:-0}"
SHOW_EVERY="${SHOW_EVERY:-1}"
LOG_STDERR="${LOG_STDERR:-1}"
DEBUG_DETECT="${DEBUG_DETECT:-0}"

# Allow positional argument: ./flaky-test-loop.sh 50
if [[ $# -ge 1 && -z "${MAX_ITER}" ]]; then
  MAX_ITER="$1"
fi
: "${MAX_ITER:=1000}"

# Patterns to detect issues in logs
BROKEN_PIPE_RE='Broken pipe'
TEST_FAILED_RE='tests failed|timeout'

# =============================================================================
# Utility Functions
# =============================================================================

trim() {
  local s="$1"
  s="${s#${s%%[![:space:]]*}}"
  s="${s%${s##*[![:space:]]}}"
  printf '%s' "$s"
}

log() {
  echo "[loop] $1" >&2
}

log_error() {
  echo "[loop][error] $1" >&2
}

# =============================================================================
# Test Pattern Handling
# =============================================================================

items=()  # Array of 'BIN::PATTERN' entries

add_item() {
  local entry="$1"
  [[ -z "$entry" ]] && return
  if [[ "$entry" == *"::"* ]]; then
    items+=("$entry")
  else
    items+=("ghcide-tests::${entry}")
  fi
}

parse_test_patterns() {
  local raw_items=() it trimmed
  IFS=',' read -r -a raw_items <<< "${TEST_PATTERNS}"
  for it in "${raw_items[@]}"; do
    trimmed=$(trim "$it")
    add_item "$trimmed"
  done
}

parse_pattern_file() {
  local line trimmed
  while IFS= read -r line || [[ -n "$line" ]]; do
    trimmed=$(trim "$line")
    [[ -z "$trimmed" || "$trimmed" == \#* ]] && continue
    add_item "$trimmed"
  done < "${PATTERN_FILE}"
}

load_test_patterns() {
  if [[ -n "${TEST_PATTERNS:-}" ]]; then
    parse_test_patterns
  elif [[ -n "${PATTERN_FILE:-}" && -r "${PATTERN_FILE}" ]]; then
    parse_pattern_file
  else
    add_item "open close"
  fi

  if [[ ${#items[@]} -eq 0 ]]; then
    log_error "No test entries provided (via PATTERN_FILE or TEST_PATTERNS)."
    exit 2
  fi
}

# =============================================================================
# Binary Path Resolution (with caching)
# =============================================================================

bin_cache_names=()
bin_cache_paths=()

get_bin_path() {
  local name="$1"
  local i

  # Check cache
  for ((i = 0; i < ${#bin_cache_names[@]}; i++)); do
    if [[ "${bin_cache_names[i]}" == "$name" ]]; then
      printf '%s\n' "${bin_cache_paths[i]}"
      return
    fi
  done

  local path
  path=$(cabal list-bin "$name" --verbose=0 2>/dev/null)

  if [[ -z "$path" ]]; then
    log_error "Unable to locate binary for '${name}' via 'cabal list-bin'."
    log_error "Try running 'cabal build ${name}' to ensure the target exists."
    exit 2
  fi

  bin_cache_names+=("$name")
  bin_cache_paths+=("$path")
  printf '%s\n' "$path"
}

# =============================================================================
# Build Step
# =============================================================================

build_test_binaries() {
  [[ -n "${NO_BUILD_ONCE:-}" ]] && return

  # Collect unique binary names
  local bins=()
  local bin already_added existing
  for item in "${items[@]}"; do
    bin="${item%%::*}"
    already_added=0
    for existing in "${bins[@]+"${bins[@]}"}"; do
      if [[ "$existing" == "$bin" ]]; then
        already_added=1
        break
      fi
    done
    if [[ $already_added -eq 0 ]]; then
      bins+=("$bin")
    fi
  done

  if (( ${#bins[@]} == 0 )); then
    return
  fi

  log "Building test targets: ${bins[*]}"
  if ! cabal build "${bins[@]}" >&2; then
    log_error "Build failed. Cannot proceed with tests."
    exit 2
  fi
  log "Build succeeded."
}

# =============================================================================
# Test Execution
# =============================================================================

run_single_test() {
  local bin_name="$1" pattern="$2" iter="$3" log_slot="$4"

  # Create log filename from sanitized pattern
  local slug
  slug=$(printf '%s' "${bin_name}-${pattern}" | tr -cs 'A-Za-z0-9._-' '-' | sed -E 's/^-+|-+$//g')
  [[ -z "${slug}" ]] && slug="pattern"
  local logfile="test-logs/${slug}-loop-${log_slot}.log"

  # Show iteration header if appropriate
  if [[ ${iter} -eq 1 || ( ${SHOW_EVERY} -gt 0 && $((iter % SHOW_EVERY)) -eq 0 ) ]]; then
    echo "[loop] Iteration ${iter} pattern='${pattern}' -> ${logfile}" | tee -a "${logfile}" >&2
  fi

  local bin_path
  bin_path=$(get_bin_path "${bin_name}")

  # Run test (don't fail on non-zero exit)
  set +e
  HLS_TEST_LOG_STDERR="${LOG_STDERR}" \
  HLS_TEST_HARNESS_STDERR="${LOG_STDERR}" \
  TASTY_NUM_THREADS=1 \
  TASTY_PATTERN="${pattern}" \
    "${bin_path}" +RTS -l -olhlint.eventlog -RTS >"${logfile}" 2>&1
  set -e

  # Check for issues
  check_for_issues "${logfile}" "${iter}" "${pattern}"
}

check_for_issues() {
  local logfile="$1" iter="$2" pattern="$3"

  if grep -aFiq -- "${BROKEN_PIPE_RE}" "${logfile}"; then
    report_issue "Broken pipe reproduced" "${logfile}" "${iter}" "${pattern}"
    exit 1
  fi

  if grep -aEq -- "${TEST_FAILED_RE}" "${logfile}"; then
    report_issue "Test failure detected" "${logfile}" "${iter}" "${pattern}"
    exit 1
  fi

  if [[ ${DEBUG_DETECT} -eq 1 ]]; then
    echo "[loop][debug] No issues in iteration ${iter} (pattern='${pattern}')." >&2
  fi
}

report_issue() {
  local message="$1" logfile="$2" iter="$3" pattern="$4"

  echo "[loop] ${message} in iteration ${iter} for pattern '${pattern}'. Stopping." | tee -a "${logfile}" >&2
  echo "[loop] Log file: ${logfile} (abs: $(pwd)/${logfile})" | tee -a "${logfile}" >&2
  echo "[loop] --- Tail (last 60 lines) ---" >&2
  tail -n 60 "${logfile}" >&2
}

# =============================================================================
# Main Loop
# =============================================================================

main() {
  mkdir -p test-logs
  load_test_patterns
  build_test_binaries

  log "Starting at $(date -Iseconds)"

  local iter=0
  while true; do
    iter=$((iter + 1))
    local log_slot=$((iter % 2))

    # Run each test pattern
    for item in "${items[@]}"; do
      local bin_name="${item%%::*}"
      local pattern="${item#*::}"
      run_single_test "${bin_name}" "${pattern}" "${iter}" "${log_slot}"
    done

    # Check iteration limit
    if [[ ${iter} -ge ${MAX_ITER} ]]; then
      log "Reached MAX_ITER=${MAX_ITER} without reproducing issues."
      exit 0
    fi

    # Progress report
    if [[ ${SHOW_EVERY} -gt 0 && $((iter % SHOW_EVERY)) -eq 0 ]]; then
      log "Progress: Completed ${iter} iterations without detecting issues."
    fi

    # Sleep between iterations
    if [[ ${SLEEP_SECS} -gt 0 ]]; then
      log "Sleeping ${SLEEP_SECS}s"
      sleep "${SLEEP_SECS}"
    fi
  done
}

main
