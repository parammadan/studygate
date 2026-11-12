# Integration tests on the shipped synthetic studies. The expected findings
# below were worked out by hand from the defect list in data-raw/make_examples.R.

spec_a <- studygate_example("study_a.yml")
spec_b <- studygate_example("study_b.yml")

test_that("study A has exactly one finding: the orphan ADAE subject", {
  res <- validate_study(study_a, spec_a)
  expect_equal(res$gate, "BLOCKED")
  expect_equal(nrow(res$findings), 1)
  f <- res$findings
  expect_equal(f$rule_id, "SGA-ADAE-004")
  expect_equal(f$key, "USUBJID=SYN-A-011; AESEQ=3")
  expect_equal(f$observed, "SYN-A-011")
  expect_true(f$blocking)
})

test_that("correcting the source record and rerunning the same rules passes", {
  fixed <- study_a
  fixed$ADAE$USUBJID[fixed$ADAE$USUBJID == "SYN-A-011"] <- "SYN-A-001"
  before <- validate_study(study_a, spec_a)
  after <- validate_study(fixed, spec_a)
  expect_equal(after$gate, "PASS")
  expect_equal(nrow(after$findings), 0)
  expect_true(all(after$rules$status == "pass"))
  cmp <- compare_results(before, after)
  expect_equal(cmp$change, "resolved")
  expect_equal(cmp$rule_id, "SGA-ADAE-004")
})

test_that("study B passes all of its own rules", {
  res <- validate_study(study_b, spec_b)
  expect_equal(res$gate, "PASS")
  expect_true(all(res$rules$status == "pass"))
})

test_that("the same engine applies different study requirements", {
  # Study B data judged by Study A's rules: ages in months fall outside the
  # adult range, the pediatric age groups and cohorts are not Study A terms,
  # and Study A requires CD4BL and AESEV, which Study B does not collect.
  res <- validate_study(study_b, spec_a)
  expect_equal(res$gate, "BLOCKED")
  failed <- res$rules$rule_id[res$rules$status != "pass"]
  expect_true(all(c("SGA-ADSL-001", "SGA-ADSL-008", "SGA-ADSL-009", "SGA-ADSL-010", "SGA-ADAE-001") %in% failed))
  miss <- res$findings$variable[res$findings$rule_id == "SGA-ADSL-001"]
  expect_equal(miss, "CD4BL")
  # The 14-day vs 30-day window matters: SYN-B-008's AE (day 100, last dose
  # day 83) is not emergent under Study B but is under Study A's 30-day window.
  te <- res$findings[res$findings$rule_id == "SGA-ADAE-009", ]
  expect_equal(te$key, "USUBJID=SYN-B-008; AESEQ=1")
  expect_equal(te$expected, "Y")
})

test_that("the defects showcase produces exactly the hand-derived findings", {
  res <- validate_study(study_a_defects, spec_a)
  got <- res$findings[, c("rule_id", "key", "variable", "expected", "observed")]
  want <- data.frame(
    rule_id = c("SGA-ADSL-002", "SGA-ADSL-005", "SGA-ADSL-006", "SGA-ADSL-009",
                "SGA-ADSL-010", "SGA-ADSL-010", "SGA-ADAE-004", "SGA-ADAE-005",
                "SGA-ADAE-006", "SGA-ADAE-008", "SGA-ADAE-009", "SGA-ADAE-009", "SGA-ADAE-009"),
    key = c(NA, "USUBJID=SYN-A-010", "USUBJID=SYN-A-006", "USUBJID=SYN-A-008",
            "USUBJID=SYN-A-007", "USUBJID=SYN-A-008", "USUBJID=SYN-A-011; AESEQ=3",
            "USUBJID=SYN-A-003; AESEQ=1", "USUBJID=SYN-A-004; AESEQ=1", "USUBJID=SYN-A-008; AESEQ=1",
            "USUBJID=SYN-A-002; AESEQ=2", "USUBJID=SYN-A-007; AESEQ=1", "USUBJID=SYN-A-010; AESEQ=1"),
    variable = c("CD4BL", "TRTSDT", "SEX", "AGE", "AGEGR1", "AGEGR1", "USUBJID", "TRTA",
                 "AESEV", "AENDT", "TRTEMFL", "TRTEMFL", "TRTEMFL"),
    expected = c("numeric", "non-missing", "one of SEX: M, F", "between 18 and 75", "35-49",
                 "(no category covers this value)", "a USUBJID present in ADSL", "Regimen 1",
                 "one of AESEV: MILD, MODERATE, SEVERE", "AENDT >= ASTDT (2025-02-26)",
                 "<missing>", "Y", "derivable flag"),
    observed = c("character", "<missing>", "Female", "17", ">=50", "18-34", "SYN-A-011",
                 "Regimen 2", "Severe", "2025-02-24", "Y", "<missing>", "cannot be derived"),
    stringsAsFactors = FALSE
  )
  expect_equal(got, want)
  # CD4BL stored as text: the CD4 range rule cannot run, and says so.
  r11 <- res$rules[res$rules$rule_id == "SGA-ADSL-011", ]
  expect_equal(r11$status, "not_evaluated")
  expect_false(r11$blocking)
})

test_that("example data are labelled synthetic in the specifications", {
  expect_true(read_spec(spec_a)$study$synthetic)
  expect_true(read_spec(spec_b)$study$synthetic)
  expect_true(all(grepl("^SYN-", c(study_a$ADSL$USUBJID, study_b$ADSL$USUBJID))))
})
