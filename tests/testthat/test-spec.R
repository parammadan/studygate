test_that("the shipped specifications load", {
  a <- read_spec(studygate_example("study_a.yml"))
  b <- read_spec(studygate_example("study_b.yml"))
  expect_s3_class(a, "studygate_spec")
  expect_equal(a$spec$version, "1.1.0")
  expect_equal(length(a$rules), 21)
  expect_equal(length(b$rules), 18)
  expect_true(all(c("ADSL", "ADAE") %in% names(b$datasets)))
  expect_match(a$hash, "^[0-9a-f]{32}$")
})

test_that("blocking defaults to severity == error and can be overridden", {
  r <- list_rules(studygate_example("study_a.yml"))
  expect_true(all(r$blocking[r$severity == "error"]))
  expect_false(r$blocking[r$id == "SGA-ADSL-011"])
})

expect_spec_problem <- function(spec, pattern) {
  err <- tryCatch(read_spec(spec), studygate_spec_error = function(e) e)
  expect_s3_class(err, "studygate_spec_error")
  expect_true(any(grepl(pattern, err$problems)), info = paste(err$problems, collapse = " | "))
  invisible(err)
}

test_that("unknown checks, duplicate ids and missing guidance are rejected", {
  s <- mk_spec(list(list(check = "no_such_check", dataset = "ADSL")))
  expect_spec_problem(s, "unknown check 'no_such_check'")
  s <- mk_spec(list(list(id = "X", check = "unique_key", dataset = "ADSL"),
                    list(id = "X", check = "unique_key", dataset = "ADAE")))
  expect_spec_problem(s, "duplicate rule id")
  s <- mk_spec(list(list(check = "unique_key", dataset = "ADSL")))
  s$rules[[1]]$why <- NULL
  expect_spec_problem(s, "'why' is required")
})

test_that("references to undeclared variables, datasets and codelists are rejected", {
  expect_spec_problem(mk_spec(list(list(check = "range", dataset = "ADSL", params = list(variable = "AGEX", min = 0)))),
                      "not declared in ADSL: AGEX")
  expect_spec_problem(mk_spec(list(list(check = "unique_key", dataset = "ADXX"))), "dataset 'ADXX' is not declared")
  expect_spec_problem(mk_spec(list(list(check = "codelist", dataset = "ADSL",
                                        params = list(variable = "SEX", codelist = "NOPE")))),
                      "codelist 'NOPE' is not defined")
  expect_spec_problem(mk_spec(list(list(check = "subject_in_parent", dataset = "ADAE",
                                        params = list(parent = "ADXX")))), "parent dataset 'ADXX'")
})

test_that("missing and unknown params are rejected", {
  expect_spec_problem(mk_spec(list(list(check = "codelist", dataset = "ADSL", params = list(variable = "SEX")))),
                      "needs param\\(s\\): codelist")
  expect_spec_problem(mk_spec(list(list(check = "range", dataset = "ADSL",
                                        params = list(variable = "AGE", min = 1, maxx = 3)))),
                      "unknown param\\(s\\).*maxx")
  expect_spec_problem(mk_spec(list(list(check = "treatment_emergent", dataset = "ADAE", params = list(
    flag = "TRTEMFL", start = "ASTDT", parent = "ADSL", trt_start = "TRTSDT", trt_end = "TRTEDT",
    window_days = -1, missing_start = "guess")))), "window_days must be")
})

test_that("unquoted Y/N in YAML (parsed as true/false) are caught", {
  path <- tempfile(fileext = ".yml")
  writeLines(c(
    "schema_version: 1", "study: {id: T}", "spec: {version: 1.0.0}",
    "codelists:", "  NY: [Y, N]",
    "datasets:", "  ADSL:", "    variables:", "      SAFFL: {type: character}",
    "rules:",
    "  - {id: R1, check: codelist, dataset: ADSL, params: {variable: SAFFL, codelist: NY},",
    "     severity: error, why: w, guidance: g, reference: r}"), path)
  expect_spec_problem(path, "quote coded values")
})

test_that("malformed where filters and bad versions are rejected", {
  s <- mk_spec(list(list(check = "not_missing", dataset = "ADSL",
                         params = list(variables = list("SEX"), where = list(variable = "SAFFL")))))
  expect_spec_problem(s, "where must be")
  s <- mk_spec(list(list(check = "unique_key", dataset = "ADSL"))); s$spec$version <- "v2"
  expect_spec_problem(s, "MAJOR.MINOR.PATCH")
})

test_that("every problem is reported at once", {
  s <- mk_spec(list(list(check = "nope", dataset = "ADSL"), list(check = "unique_key", dataset = "ADXX")))
  s$spec$version <- NULL
  err <- expect_spec_problem(s, "spec.version is required")
  expect_gte(length(err$problems), 3)
})

test_that("missing files and invalid YAML give clear errors", {
  expect_spec_problem(file.path(tempdir(), "nope.yml"), "not found")
  bad <- tempfile(fileext = ".yml"); writeLines("rules: [unclosed", bad)
  expect_spec_problem(bad, "YAML could not be parsed")
})

test_that("list_checks documents every check type", {
  ch <- list_checks()
  expect_equal(nrow(ch), 11)
  expect_true(all(nzchar(ch$description)))
})
