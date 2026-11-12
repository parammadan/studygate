#!/usr/bin/env bash
# Runs the full test suite and writes a one-line stamped summary to
# artifacts/test-summary.txt (used by the demo video). Fails on any failure or skip.
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p artifacts
commit=$(git rev-parse --short HEAD 2>/dev/null || echo "no-git")
Rscript -e '
  r <- as.data.frame(testthat::test_local(".", reporter = "silent", stop_on_failure = FALSE))
  bad <- sum(r$failed) + sum(r$error)
  line <- sprintf("testthat: %d tests | %d expectations | %d failed | %d skipped",
                  nrow(r), sum(r$nb), bad, sum(r$skipped))
  writeLines(line, "artifacts/test-summary.tmp")
  if (bad + sum(r$skipped) > 0) quit(status = 1)' 2>/dev/null
{ echo "studygate $(grep '^Version' DESCRIPTION | cut -d' ' -f2) @ ${commit} | run $(date '+%Y-%m-%d %H:%M %Z')"
  cat artifacts/test-summary.tmp; } > artifacts/test-summary.txt
rm artifacts/test-summary.tmp
cat artifacts/test-summary.txt
