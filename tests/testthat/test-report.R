spec_a <- studygate_example("study_a.yml")

test_that("the console report shows status, finding, guidance and gate", {
  out <- capture.output(report_validation(validate_study(study_a, spec_a)))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "[FAIL] SGA-ADAE-004", fixed = TRUE)
  expect_match(txt, "ADAE row 3 (USUBJID=SYN-A-011; AESEQ=3)", fixed = TRUE)
  expect_match(txt, "Why it matters:", fixed = TRUE)
  expect_match(txt, "Investigate:", fixed = TRUE)
  expect_match(txt, "adae[!adae$USUBJID %in% adsl$USUBJID, ]", fixed = TRUE)
  expect_match(txt, "GATE: BLOCKED", fixed = TRUE)
})

test_that("the console report explains rules that could not run", {
  out <- paste(capture.output(report_validation(validate_study(study_a_defects, spec_a))), collapse = "\n")
  expect_match(out, "NOT EVALUATED. ADSL.CD4BL must be numeric", fixed = TRUE)
  expect_match(out, "A rule that cannot run is not a pass", fixed = TRUE)
})

test_that("the printed R code really returns the affected rows", {
  adae <- study_a$ADAE; adsl <- study_a$ADSL
  rows <- adae[!adae$USUBJID %in% adsl$USUBJID, ]
  expect_equal(rows$USUBJID, "SYN-A-011")
})

test_that("write_validation_report writes HTML, CSV and JSON evidence", {
  res <- validate_study(study_a_defects, spec_a)
  dir <- withr::local_tempdir()
  paths <- write_validation_report(res, dir)
  expect_true(all(file.exists(paths)))
  ev <- jsonlite::read_json(paths[["evidence"]])
  expect_equal(ev$gate, "BLOCKED")
  expect_equal(ev$spec_version, "1.1.0")
  expect_equal(ev$n_findings, 13)
  expect_equal(ev$rule_status$`SGA-ADSL-011`, "not_evaluated")
  expect_equal(ev$evidence_id, res$evidence$evidence_id)
  expect_true(ev$synthetic_data)
  f <- utils::read.csv(paths[["findings"]], stringsAsFactors = FALSE)
  expect_equal(nrow(f), 13)
  html <- paste(readLines(paths[["html"]]), collapse = "\n")
  expect_match(html, "GATE: BLOCKED", fixed = TRUE)
  expect_match(html, "&lt;missing&gt;", fixed = TRUE)       # escaped, not raw markup
  expect_false(grepl("<missing>", html, fixed = TRUE))
})

test_that("a passing run reports no findings", {
  dir <- withr::local_tempdir()
  paths <- write_validation_report(validate_study(study_b, studygate_example("study_b.yml")), dir)
  html <- paste(readLines(paths[["html"]]), collapse = "\n")
  expect_match(html, "GATE: PASS", fixed = TRUE)
  expect_match(html, "No findings", fixed = TRUE)
  expect_equal(nrow(utils::read.csv(paths[["findings"]])), 0)
})

test_that("explain_rule shows rule, provenance, R code and a SAS analogue", {
  out <- paste(capture.output(explain_rule(spec_a, "SGA-ADAE-009")), collapse = "\n")
  expect_match(out, "rule added in v1.1.0", fixed = TRUE)
  expect_match(out, "window_days: 30", fixed = TRUE)
  expect_match(out, "statistical analysis plan v2.0", fixed = TRUE)
  expect_match(out, "SAS analogue (illustrative, not executed by studygate)", fixed = TRUE)
  expect_error(explain_rule(spec_a, "NOPE"), "no rule 'NOPE'")
})

test_that("explain_rule accepts a result as well as a specification", {
  res <- validate_study(study_a, spec_a)
  expect_output(explain_rule(res, "SGA-ADAE-004"), "proc sql; select \\* from ADAE")
})

test_that("every generated R snippet runs and returns exactly the flagged rows", {
  # The guidance is only useful if the code it prints is right. Run each
  # snippet against the defects data and compare with the engine's findings.
  spec <- read_spec(spec_a)
  res <- validate_study(study_a_defects, spec)
  env <- list2env(list(adsl = study_a_defects$ADSL, adae = study_a_defects$ADAE))
  row_level <- c("not_missing", "codelist", "range", "date_order", "subject_in_parent", "unique_key")
  for (rule in spec$rules) {
    code <- sg_checks[[rule$check]]$r_code(rule, spec)
    out <- eval(parse(text = code), envir = new.env(parent = env))
    evaluated <- res$rules$status[res$rules$rule_id == rule$id] != "not_evaluated"
    if (rule$check %in% row_level && evaluated) {
      f <- res$findings[res$findings$rule_id == rule$id, ]
      expect_equal(nrow(out), nrow(f), info = paste(rule$id, code))
      if (nrow(f)) {
        expect_setequal(out$USUBJID, sub("^USUBJID=([^;]*).*$", "\\1", f$key))
      }
    }
  }
})
