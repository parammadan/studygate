ds <- function() list(ADSL = adsl0(), ADAE = adae0())
spec_two <- function(second_blocking = TRUE, second_severity = "error") {
  mk_spec(list(
    list(check = "unique_key", dataset = "ADSL"),
    list(check = "range", dataset = "ADSL", severity = second_severity, blocking = second_blocking,
         params = list(variable = "AGE", max = 50))     # S-3 (60) fails
  ))
}

test_that("a failing blocking rule blocks the gate", {
  res <- validate_study(ds(), spec_two())
  expect_equal(res$gate, "BLOCKED")
  expect_false(gate_passed(res))
  expect_equal(res$rules$status, c("pass", "fail"))
})

test_that("a failing non-blocking warning does not block the gate", {
  res <- validate_study(ds(), spec_two(second_blocking = FALSE, second_severity = "warning"))
  expect_equal(res$gate, "PASS")
  expect_equal(nrow(res$findings), 1)
})

test_that("a rule that cannot run is not a pass and blocks when blocking", {
  res <- validate_study(list(ADSL = adsl0()),
                        mk_spec(list(list(check = "unique_key", dataset = "ADAE"))))
  expect_equal(res$rules$status, "not_evaluated")
  expect_match(res$rules$detail, "ADAE was not supplied")
  expect_equal(res$gate, "BLOCKED")
})

test_that("an unexpected crash inside a check is recorded as error, not pass", {
  local_mocked_bindings(sg_require_columns = function(...) stop("boom"))
  res <- validate_study(ds(), mk_spec(list(list(check = "unique_key", dataset = "ADSL"))))
  expect_equal(res$rules$status, "error")
  expect_match(res$rules$detail, "boom")
  expect_equal(res$gate, "BLOCKED")
})

test_that("bad inputs raise structured errors", {
  s <- spec_two()
  expect_error(validate_study(adsl0(), s), class = "studygate_input_error")
  expect_error(validate_study(list(adsl0()), s), class = "studygate_input_error")
  expect_error(validate_study(list(ADSL = "x"), s), class = "studygate_input_error")
  expect_error(validate_study(list(ADSL = adsl0(), adsl = adsl0()), s), class = "studygate_input_error")
})

test_that("dataset names match without regard to case, and tibble-like inputs work", {
  res <- validate_study(list(adsl = adsl0()), spec_two())
  expect_equal(res$rules$status, c("pass", "fail"))
  tb <- adsl0(); class(tb) <- c("tbl_df", "tbl", "data.frame")
  expect_equal(validate_study(list(ADSL = tb), spec_two())$rules$status, c("pass", "fail"))
})

test_that("input data are never modified", {
  d <- ds(); before <- d
  validate_study(d, studygate_example("study_a.yml"))
  expect_identical(d, before)
})

test_that("results are deterministic and the evidence id ignores the run time", {
  s <- studygate_example("study_a.yml")
  r1 <- validate_study(study_a_defects, s, run_time = as.POSIXct("2026-01-01", tz = "UTC"))
  r2 <- validate_study(study_a_defects, s, run_time = as.POSIXct("2026-06-01", tz = "UTC"))
  expect_identical(r1$findings, r2$findings)
  expect_identical(r1$rules, r2$rules)
  expect_identical(r1$evidence$evidence_id, r2$evidence$evidence_id)
  expect_false(identical(r1$evidence$run_time, r2$evidence$run_time))
})

test_that("the evidence id changes when the data change", {
  s <- studygate_example("study_a.yml")
  fixed <- study_a; fixed$ADAE$USUBJID[3] <- "SYN-A-001"
  expect_false(identical(validate_study(study_a, s)$evidence$evidence_id,
                         validate_study(fixed, s)$evidence$evidence_id))
})

test_that("findings carry rule provenance and guidance", {
  res <- validate_study(study_a, studygate_example("study_a.yml"))
  f <- res$findings
  expect_named(f, c("rule_id", "dataset", "severity", "blocking", "row", "key", "variable",
                    "expected", "observed", "message", "why", "guidance", "reference"))
  expect_true(all(nzchar(c(f$why, f$guidance, f$reference))))
  expect_identical(as.data.frame(res), f)
})
