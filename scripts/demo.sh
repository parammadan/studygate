#!/usr/bin/env bash
# StudyGate end-to-end demo on SYNTHETIC data. Every line of output comes from
# the commands below. Set DEMO_NO_PAUSE=1 to run without pauses (as CI does).
set -euo pipefail
cd "$(dirname "$0")/.."
SG=./bin/studygate
SPECS=inst/extdata/specs
DATA=inst/extdata/studies
pause() { [ -n "${DEMO_NO_PAUSE:-}" ] || read -rp "  [enter to continue] " _; }
step() { printf '\n\033[1m== %s\033[0m\n' "$1"; }

rm -rf artifacts && mkdir -p artifacts

step "1. One package, two studies with different versioned specifications"
scripts/spec_summary.R
pause

step "2. Study B (pediatric): validate SAS transport files against its own rules"
$SG validate --spec $SPECS/study_b.yml --data $DATA/study_b | tail -n 3
pause

step "3. Study A (adult): an ADAE record whose subject is not in ADSL"
set +e
$SG validate --spec $SPECS/study_a.yml --data $DATA/study_a --out artifacts/report_before
code=$?
set -e
echo "exit code: $code   (2 = gate blocked)"
test "$code" -eq 2
pause

step "4. Programmer guidance for the failed rule"
$SG explain --spec $SPECS/study_a.yml --rule SGA-ADAE-004
pause

step "5. Correct a copy of the source data after the query is answered"
scripts/apply_correction.R $DATA/study_a artifacts/study_a_corrected
pause

step "6. Rerun the same rules on the corrected data"
$SG validate --spec $SPECS/study_a.yml --data artifacts/study_a_corrected --out artifacts/report_after | tail -n 4
pause

step "7. Evidence for the passing run"
grep -E '"(study_id|spec_version|gate|n_findings|spec_md5|ADSL|ADAE|package_version|evidence_id)"' artifacts/report_after/evidence.json
