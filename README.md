# StudyGate R

**Study-specific validation rules and programmer guidance for clinical analysis datasets.**

`studygate` is an R package that turns a versioned, study-specific YAML specification into
deterministic checks on ADaM-style analysis datasets (for example ADSL and ADAE). For every
finding it explains what is wrong, which record is affected, why it matters, and how to
investigate it. The same rules drive validation, the release gate, and the guidance a
programmer reads. A new study reuses the engine by supplying a new specification.

> **Independent research prototype.** Not affiliated with or endorsed by Harvard, the Harvard
> T.H. Chan School of Public Health, CBAR, ACTG, IMPAACT, CDISC or PharmaSUG. It is not a validated
> or regulatory-certified tool and not a CDISC conformance checker. **All data in this
> repository are synthetic**: hand-written, invented records with no real patient information.

```r
library(studygate)

result <- validate_study(
  datasets = list(ADSL = adsl, ADAE = adae),
  specification = "study_a.yml"
)
report_validation(result)
```

## Where it fits

```
raw data ──► SDTM ──► ADaM (ADSL, ADAE, …) ──► [ StudyGate ] ──► tables, figures, listings
                                                    │
                       versioned study spec (YAML) ─┘   findings + guidance + evidence + gate
```

StudyGate answers one question **before analysis**: *do these analysis datasets meet this
study's explicit requirements, and if not, what exactly should the programmer look at?*

## Install

```r
# from a clone of this repository
install.packages(c("yaml", "jsonlite"))        # imports
install.packages("haven")                       # optional: read .xpt / .sas7bdat
devtools::install()                             # or: R CMD INSTALL .
```

R ≥ 4.1. Runtime imports are only `yaml` and `jsonlite` (plus base `tools`/`utils`).

## A 60-second tour

```r
library(studygate)

spec <- read_spec(studygate_example("study_a.yml"))   # versioned, checked up front
res  <- validate_study(study_a, spec)                  # synthetic example data
res
#> StudyGate | SYN-HIV-A | spec v1.1.0 | evidence 3e9f9cfa9493
#> Rules: 21  pass 20  fail 1  not evaluated 0  error 0  | findings: 1
#> GATE: BLOCKED

report_validation(res)          # every rule, then each failure with guidance
explain_rule(spec, "SGA-ADAE-004")   # rule card: why, how to investigate, R + SAS hints

fixed <- study_a                # after the data query is answered, correct the source…
fixed$ADAE$USUBJID[fixed$ADAE$USUBJID == "SYN-A-011"] <- "SYN-A-001"
res2 <- validate_study(fixed, spec)   # …and rerun the same rules
compare_results(res, res2)      # 1 resolved, 0 new
write_validation_report(res2, "report/")   # report.html, findings.csv, rules.csv, evidence.json
```

What a finding looks like (real output):

```
SGA-ADAE-004  Every ADAE subject exists in ADSL  [error, blocks release]
  * ADAE row 3 (USUBJID=SYN-A-011; AESEQ=3)
      USUBJID SYN-A-011 has ADAE records but no ADSL record | expected: a USUBJID present in ADSL | observed: SYN-A-011
  Why it matters: An adverse event whose subject is not in ADSL has no
    treatment arm, population flag or dosing dates, so it is silently
    dropped from every safety table.
  Investigate: Check the subject identifier in the source AE data for a
    typo or transposition, and confirm with data management whether the
    subject was enrolled. Do not edit the analysis dataset by hand;
    correct the source and rerun.
  Reference: Study A analysis data specification v1.1.0, section 3.3 (synthetic)
  See the rows in R: adae[!adae$USUBJID %in% adsl$USUBJID, ]
```

## Command line (Linux / Bash)

```sh
./bin/studygate validate --spec inst/extdata/specs/study_a.yml \
                         --data inst/extdata/studies/study_a --out report/
echo $?          # 0 = gate PASS, 2 = gate BLOCKED, 1 = specification or input error
./bin/studygate explain --spec inst/extdata/specs/study_a.yml --rule SGA-ADAE-004
./bin/studygate rules   --spec inst/extdata/specs/study_b.yml
./bin/studygate checks
```

The data folder holds one file per dataset (`adsl.xpt`, `adae.sas7bdat`, `adsl.rds` or
`adae.csv`). The exit codes make it usable as a gate in a batch job or CI pipeline.

## One engine, two studies

Both example studies use the same 11 check types. Only the specifications differ:

| | Study A: synthetic adult HIV treatment | Study B: synthetic pediatric HIV |
|---|---|---|
| Specification | `study_a.yml` v1.1.0, 21 rules | `study_b.yml` v1.0.0, 18 rules |
| Age | 18–75 **years** | 0–215 **months** |
| Age groups | 18-34 · 35-49 · ≥50 | <2 · 2-<6 · 6-<12 · 12-<18 years |
| AE intensity | `AESEV`: MILD / MODERATE / SEVERE | `AETOXGR`: grade 1–5 |
| TEAE window | last dose + 30 days | last dose + 14 days |
| Study-only rule | baseline CD4 plausibility (warning) | baseline weight required for weight-band dosing |

Judging Study B's data by Study A's rules fails in the expected places. This is tested: an
AE on day 100 after a day-83 last dose is treatment-emergent under Study A's 30-day window
but not under Study B's 14-day window.

## A specification

```yaml
schema_version: 1
study: {id: SYN-HIV-A, title: Synthetic adult HIV treatment study, synthetic: true}
spec:
  version: 1.1.0
  changes:
    - {version: 1.1.0, date: "2026-11-01", summary: TEAE window confirmed as last dose + 30 days}
codelists:
  NY: ["Y", "N"]                 # quote coded values: YAML reads bare Y/N as true/false
datasets:
  ADAE:
    key: [USUBJID, AESEQ]
    variables:
      USUBJID: {type: character, required: true}
      ASTDT:   {type: date,      required: true}
      # …
rules:
  - id: SGA-ADAE-004
    title: Every ADAE subject exists in ADSL
    check: subject_in_parent
    dataset: ADAE
    params: {parent: ADSL, by: USUBJID}
    severity: error              # errors block release by default; `blocking:` overrides
    since: 1.0.0
    why: An adverse event whose subject is not in ADSL … is silently dropped from every safety table.
    guidance: Check the subject identifier in the source AE data for a typo or transposition …
    reference: Study A analysis data specification v1.1.0, section 3.3 (synthetic)
```

## Check types

| Check | What it verifies |
|---|---|
| `required_variables` | required variables are present |
| `variable_types` | declared type (character / numeric / date) matches the data |
| `not_missing` | no NA or blank values, optionally within a subset (`where: {variable: SAFFL, equals: "Y"}`) |
| `unique_key` | key variables identify each record and are never missing |
| `codelist` | values come from a study codelist (exact, case-sensitive, with a "did you mean" hint) |
| `range` | numeric values within [min, max] |
| `subject_in_parent` | every record's subject exists in the subject-level dataset |
| `parent_consistency` | carried variables equal the parent's (e.g. `ADAE.TRTA = ADSL.TRT01A`) |
| `date_order` | end date not before start date |
| `treatment_emergent` | the TEAE flag matches the study's approved rule (window, missing-start convention) |
| `categorization` | a derived group (e.g. `AGEGR1`) agrees with its source and cut points |

## Design decisions

- **A rule that cannot run is never a pass.** If a variable a rule needs is absent or has the
  wrong type, the rule is `not_evaluated`, and that blocks the gate if the rule is blocking.
  A crash inside a check is recorded as `error`, never silently skipped.
- **Data are never modified.** Not by validation, and not by the CSV reader. A column is typed
  from the spec only if *every* value converts cleanly; otherwise it stays as text and is
  reported by `variable_types`. Nothing is coerced to missing.
- **SAS-aware missing values.** SAS has no separate character missing value, so a blank
  character variable read from `.xpt` arrives as `""`. StudyGate treats `NA`, `""` and blanks
  alike, so results are the same whether data came from SAS or R.
- **Specifications are data, never code.** Filters are structured (`where: {variable, equals}`),
  and the engine never calls `eval()` on specification content. `read_spec()` rejects unknown
  checks or params, undeclared variables, missing guidance, duplicate IDs and unquoted `Y`/`N`,
  and reports every problem at once.
- **Provenance and evidence.** Each rule carries `reference` and `since`. Each run records the
  spec's MD5, per-dataset fingerprints, package and R versions. The evidence id is a hash of
  spec + data + package version: the same inputs always give the same id.
- **Guidance you can run.** Every rule prints R code that shows the affected rows, plus an
  illustrative SAS analogue for programmers moving from SAS. The R snippets are tested: each one
  is executed against the defect data and must return exactly the flagged records. The SAS lines
  are orientation hints only and are not executed.
- **Small dependency footprint.** Only `yaml` and `jsonlite` at runtime. `haven` is optional.
  No `dplyr` or Shiny dependency in the core.

## Testing

```r
devtools::test()     # 80 tests, 236 expectations, 0 failures, 0 skips (local run, 2026-11-12)
```

- Every check type has tests for normal input, invalid input, missing values and edge cases
  (inclusive/exclusive bounds, first-dose day, last dose + window, SAS blanks, composite keys,
  duplicated parent keys). Expectations are worked out by hand on small inline data frames.
- `study_a_defects` contains one documented defect per rule type (see `data-raw/make_examples.R`).
  The test asserts the exact 13 findings, derived by hand from that list.
- **Mutation check.** Six bugs were injected one at a time: off-by-one bounds, TEAE window `<`
  for `<=`, flipped category edges, blanks not treated as missing, not-evaluated counted as
  pass, wrong date comparison. Each one made the suite fail.
- `R CMD check --no-manual`: Status OK (0 errors, 0 warnings, 0 notes) on macOS, R 4.6.1.
  The CI workflow (`.github/workflows/ci.yml`) is configured to run R CMD check on Ubuntu and
  macOS, the full suite with no skips allowed, the CLI exit codes, and the demo script.

## Demo

```sh
DEMO_NO_PAUSE=1 ./scripts/demo.sh   # runs the whole story; drop the variable to pause per step
```

1. Two specifications side by side (values read from the YAML).
2. Study B passes its own rules.
3. Study A is blocked by an ADAE record whose subject is not in ADSL.
4. The rule card explains it, with R and SAS hints.
5. A documented correction is applied to a *copy* of the source data
   (`scripts/apply_correction.R` stops unless exactly one record matches).
6. The same rules are rerun: gate PASS.
7. Evidence JSON for the passing run.

The recorded video (`demo/StudyGate_Demo.mp4`) is built from real terminal sessions by
`python3 demo/build_video.py` (see `demo/narration.md`).

## How this differs from existing tools

| Tool | Focus | Relationship |
|---|---|---|
| Pinnacle 21 / CDISC conformance rules | standard conformance of SDTM/ADaM | StudyGate does **not** check CDISC conformance. It checks *study-specific* analysis-readiness rules. |
| `pointblank`, `validate` | general-purpose data validation | General engines. StudyGate is domain-specific: cross-dataset clinical checks (subject in ADSL, TEAE derivation, carried variables), release-gate semantics, and per-rule clinical guidance. |
| `sdtmchecks` | a library of SDTM data-quality checks | Works on SDTM. StudyGate works on analysis datasets, driven by a per-study spec. |
| `metacore` / `metatools` / `xportr` | dataset metadata from specs; attribute checks and export | Complementary. A future version could read variable metadata from a `metacore` object instead of YAML. |
| `admiral` | ADaM derivation | Derives data. StudyGate checks the derived data. |
| PharmaSUG 2026 DS-280 (Sheth & Alias) | SAS/R toolkit that *generates* SDTM from raw EDC data | Different stage and purpose. StudyGate shares no code or design with it. |

## Not implemented (future ideas)

- Reading variable metadata from `metacore` / define-like spreadsheets instead of YAML.
- Partial-date handling rules (`--DTC` imputation flags) and BDS datasets (ADLB, ADVS).
- Spec diffing between versions (which rules changed between v1.0.0 and v1.1.0).
- An optional Shiny viewer, kept outside the core package.
- Rule qualification metadata (who reviewed a rule, when) for a formal validation process.

## Repository layout

```
R/                 spec.R (parse + check spec) · checks.R (11 check types) · engine.R ·
                   report.R (console, HTML/CSV/JSON, rule cards) · io.R (xpt/sas7bdat/rds/csv) · cli.R
inst/extdata/      specs/study_a.yml, study_b.yml · studies/<study>/{adsl,adae}.xpt (synthetic)
data/ data-raw/    synthetic study_a, study_b, study_a_defects + the script that writes them
tests/testthat/    checks, spec, engine, examples, report, io/cli
vignettes/         getting started
bin/studygate      command line · scripts/ demo + helpers · demo/ video build
```

## License

MIT © 2026 Param Madan
