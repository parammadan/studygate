spec_a <- studygate_example("study_a.yml")
spec_b <- studygate_example("study_b.yml")
xpt_dir <- function(s) system.file("extdata", "studies", s, package = "studygate")

test_that("SAS transport files round-trip to the same validation result", {
  skip_if_not_installed("haven")
  d <- read_study_datasets(xpt_dir("study_a"), spec_a)
  expect_named(d, c("ADSL", "ADAE"))
  expect_s3_class(d$ADSL$TRTSDT, "Date")
  r_xpt <- validate_study(d, spec_a)
  r_rda <- validate_study(study_a, spec_a)
  expect_equal(r_xpt$rules$status, r_rda$rules$status)
  expect_equal(r_xpt$findings$key, r_rda$findings$key)
})

test_that("blank SAS character values are treated as missing", {
  skip_if_not_installed("haven")
  d <- read_study_datasets(xpt_dir("study_a"), spec_a)
  # SYN-A-009 has a blank TRT01A; it arrives as "" from the .xpt file.
  expect_equal(d$ADSL$TRT01A[d$ADSL$USUBJID == "SYN-A-009"], "")
  expect_equal(validate_study(d, spec_a)$rules$status[5], "pass")   # SGA-ADSL-005 where SAFFL = Y
})

test_that("CSV columns are typed from the spec only when every value converts", {
  dir <- withr::local_tempdir()
  adsl <- study_a$ADSL
  adsl_csv <- adsl; adsl_csv$TRTSDT <- format(adsl_csv$TRTSDT); adsl_csv$TRTEDT <- format(adsl_csv$TRTEDT)
  adsl_csv$TRTSDT[is.na(adsl$TRTSDT)] <- ""
  adsl_csv$TRTEDT[is.na(adsl$TRTEDT)] <- ""
  adsl_csv$CD4BL <- as.character(adsl_csv$CD4BL)
  adsl_csv$CD4BL[4] <- "<50"                                       # cannot be numeric
  adsl_csv$TRTEDT[1] <- "07/06/2025"                               # not ISO 8601
  utils::write.csv(adsl_csv, file.path(dir, "adsl.csv"), row.names = FALSE)
  d <- read_study_datasets(dir, spec_a)
  expect_s3_class(d$ADSL$TRTSDT, "Date")
  expect_true(is.na(d$ADSL$TRTSDT[9]))
  expect_type(d$ADSL$AGE, "double")
  expect_type(d$ADSL$CD4BL, "character")                          # left as text, not coerced to NA
  expect_type(d$ADSL$TRTEDT, "character")
  res <- validate_study(d, spec_a)
  f <- res$findings[res$findings$rule_id == "SGA-ADSL-002", ]
  expect_setequal(f$variable, c("CD4BL", "TRTEDT"))
})

test_that("an unreadable data folder is an error", {
  expect_error(read_study_datasets(file.path(tempdir(), "nope"), spec_a), "not found")
})

cli <- function(...) {
  code <- NULL
  out <- capture.output(code <- studygate_cli(c(...)), type = "output")
  list(code = code, out = paste(out, collapse = "\n"))
}

test_that("CLI exit codes: 2 for a blocked gate, 0 for a pass, 1 for errors", {
  skip_if_not_installed("haven")
  expect_equal(cli("validate", "--spec", spec_a, "--data", xpt_dir("study_a"))$code, 2L)
  expect_equal(cli("validate", "--spec", spec_b, "--data", xpt_dir("study_b"))$code, 0L)
  expect_equal(suppressMessages(cli("validate", "--spec", spec_a))$code, 1L)
  expect_equal(cli("frobnicate")$code, 1L)
  bad <- tempfile(fileext = ".yml"); writeLines("schema_version: 2", bad)
  expect_equal(cli("rules", "--spec", bad)$code, 1L)
})

test_that("CLI writes reports and explains rules", {
  skip_if_not_installed("haven")
  out <- withr::local_tempdir()
  r <- cli("validate", "--spec", spec_b, "--data", xpt_dir("study_b"), "--out", out)
  expect_true(file.exists(file.path(out, "report.html")))
  expect_match(cli("explain", "--spec", spec_a, "--rule", "SGA-ADAE-004")$out, "SAS analogue")
  expect_match(cli("rules", "--spec", spec_b)$out, "SGB-ADAE-008")
  expect_match(cli("checks")$out, "treatment_emergent")
})
