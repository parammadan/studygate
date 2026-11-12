# Each check type: normal input, invalid input, missing values, edge cases.
# Expected results are worked out by hand from the inline data.

ds <- function(adsl = adsl0(), adae = adae0()) list(ADSL = adsl, ADAE = adae)

# ---- required_variables ----
test_that("required_variables passes when every required variable is present", {
  r <- run1(list(check = "required_variables", dataset = "ADSL"), ds())
  expect_equal(r$status, "pass")
})

test_that("required_variables reports each absent required variable", {
  a <- adsl0(); a$SEX <- NULL; a$AGE <- NULL
  r <- run1(list(check = "required_variables", dataset = "ADSL"), ds(adsl = a))
  expect_equal(r$status, "fail")
  expect_setequal(r$findings$variable, c("AGE", "SEX"))
  expect_true(all(is.na(r$findings$row)))
})

test_that("required_variables ignores absent optional variables and honours an explicit list", {
  a <- adsl0(); a$AGEGR1 <- NULL                      # AGEGR1 is not required
  expect_equal(run1(list(check = "required_variables", dataset = "ADSL"), ds(adsl = a))$status, "pass")
  r <- run1(list(check = "required_variables", dataset = "ADSL",
                 params = list(variables = list("AGEGR1"))), ds(adsl = a))
  expect_equal(r$findings$variable, "AGEGR1")
})

# ---- variable_types ----
test_that("variable_types passes for matching types", {
  expect_equal(run1(list(check = "variable_types", dataset = "ADSL"), ds())$status, "pass")
})

test_that("variable_types flags text dates, text numbers and factors", {
  a <- adsl0()
  a$TRTSDT <- format(a$TRTSDT)                         # ISO text, not a Date
  a$AGE <- as.character(a$AGE)
  a$SEX <- factor(a$SEX)
  r <- run1(list(check = "variable_types", dataset = "ADSL"), ds(adsl = a))
  f <- r$findings[order(r$findings$variable), ]
  expect_equal(f$variable, c("AGE", "SEX", "TRTSDT"))
  expect_equal(f$expected, c("numeric", "character", "date"))
  expect_equal(f$observed, c("character", "factor", "character"))
})

test_that("variable_types treats integer as numeric and an all-NA logical column as wrong", {
  a <- adsl0(); a$AGE <- as.integer(a$AGE)
  expect_equal(run1(list(check = "variable_types", dataset = "ADSL"), ds(adsl = a))$status, "pass")
  a$AGEGR1 <- NA                                        # logical NA column
  r <- run1(list(check = "variable_types", dataset = "ADSL"), ds(adsl = a))
  expect_equal(r$findings$observed, "logical")
})

# ---- not_missing ----
test_that("not_missing passes with complete values", {
  r <- run1(list(check = "not_missing", dataset = "ADSL", params = list(variables = list("USUBJID", "SEX"))), ds())
  expect_equal(r$status, "pass")
})

test_that("not_missing treats NA, empty and blank-only text as missing (SAS blanks)", {
  a <- adsl0(); a$SEX <- c(NA, "", "   ")
  r <- run1(list(check = "not_missing", dataset = "ADSL", params = list(variables = list("SEX"))), ds(adsl = a))
  expect_equal(r$findings$row, 1:3)
  expect_equal(r$findings$key, c("USUBJID=S-1", "USUBJID=S-2", "USUBJID=S-3"))
})

test_that("not_missing with where only checks the subset", {
  # S-3 is SAFFL = N and has no TRTSDT: allowed. Removing S-2's date is not.
  a <- adsl0()
  rule <- list(check = "not_missing", dataset = "ADSL",
               params = list(variables = list("TRTSDT"), where = list(variable = "SAFFL", equals = "Y")))
  expect_equal(run1(rule, ds(adsl = a))$status, "pass")
  a$TRTSDT[2] <- NA
  r <- run1(rule, ds(adsl = a))
  expect_equal(r$findings$row, 2L)
})

test_that("not_missing is not evaluated when the variable is absent", {
  a <- adsl0(); a$SEX <- NULL
  r <- run1(list(check = "not_missing", dataset = "ADSL", params = list(variables = list("SEX"))), ds(adsl = a))
  expect_equal(r$status, "not_evaluated")
  expect_match(r$detail, "SEX")
})

# ---- unique_key ----
test_that("unique_key passes for unique keys", {
  expect_equal(run1(list(check = "unique_key", dataset = "ADSL"), ds())$status, "pass")
  expect_equal(run1(list(check = "unique_key", dataset = "ADAE"), ds())$status, "pass")
})

test_that("unique_key reports every row of a duplicated key with its count", {
  a <- rbind(adsl0(), adsl0()[2, ])
  r <- run1(list(check = "unique_key", dataset = "ADSL"), ds(adsl = a))
  expect_equal(r$findings$row, c(2L, 4L))
  expect_equal(r$findings$observed, c("2 records", "2 records"))
})

test_that("unique_key uses composite keys and reports missing key values", {
  e <- adae0(); e$AESEQ[2] <- 1                        # S-1 AESEQ 1 twice
  e$USUBJID[3] <- ""                                   # blank key
  r <- run1(list(check = "unique_key", dataset = "ADAE"), ds(adae = e))
  expect_equal(sort(r$findings$row), 1:3)
  expect_equal(r$findings$observed[r$findings$row == 3], "key value missing")
})

# ---- codelist ----
test_that("codelist passes for valid terms and skips missing values", {
  a <- adsl0(); a$SAFFL[3] <- ""
  r <- run1(list(check = "codelist", dataset = "ADSL", params = list(variable = "SAFFL", codelist = "NY")), ds(adsl = a))
  expect_equal(r$status, "pass")
})

test_that("codelist is case-sensitive and suggests the matching term", {
  e <- adae0(); e$AESEV[2] <- "Severe"; e$AESEV[3] <- "GRADE 2"
  r <- run1(list(check = "codelist", dataset = "ADAE", params = list(variable = "AESEV", codelist = "SEV")),
            ds(adae = e), codelists = list(SEV = c("MILD", "MODERATE", "SEVERE")))
  expect_equal(r$findings$row, 2:3)
  expect_match(r$findings$message[1], 'did you mean "SEVERE"', fixed = TRUE)
  expect_false(grepl("did you mean", r$findings$message[2]))
})

test_that("codelist matches numeric codes written in YAML", {
  e <- adae0(); e$AESEV <- c("1", "2", "6")
  r <- run1(list(check = "codelist", dataset = "ADAE", params = list(variable = "AESEV", codelist = "GR")),
            ds(adae = e), codelists = list(GR = 1:5))
  expect_equal(r$findings$observed, "6")
})

# ---- range ----
test_that("range is inclusive at both ends", {
  rule <- list(check = "range", dataset = "ADSL", params = list(variable = "AGE", min = 30, max = 60))
  expect_equal(run1(rule, ds())$status, "pass")        # 30 and 60 sit on the bounds
  a <- adsl0(); a$AGE <- c(29.9, 45, 60.1)
  expect_equal(run1(rule, ds(adsl = a))$findings$row, c(1L, 3L))
})

test_that("range skips missing values and supports one-sided bounds", {
  a <- adsl0(); a$AGE <- c(NA, 200, 5)
  r <- run1(list(check = "range", dataset = "ADSL", params = list(variable = "AGE", min = 18)), ds(adsl = a))
  expect_equal(r$findings$row, 3L)
})

test_that("range is not evaluated on a text column", {
  a <- adsl0(); a$AGE <- c("30", "<18", "60")
  r <- run1(list(check = "range", dataset = "ADSL", params = list(variable = "AGE", min = 18)), ds(adsl = a))
  expect_equal(r$status, "not_evaluated")
  expect_match(r$detail, "must be numeric")
})

# ---- subject_in_parent ----
sip <- list(check = "subject_in_parent", dataset = "ADAE", params = list(parent = "ADSL"))

test_that("subject_in_parent passes when every subject is in ADSL", {
  expect_equal(run1(sip, ds())$status, "pass")
})

test_that("subject_in_parent finds orphans and missing identifiers", {
  e <- adae0(); e$USUBJID <- c("S-1", "S-9", NA)
  r <- run1(sip, ds(adae = e))
  expect_equal(r$findings$row, 2:3)
  expect_equal(r$findings$observed, c("S-9", "<missing>"))
  expect_match(r$findings$message[1], "no ADSL record")
})

test_that("subject_in_parent does not match a blank ADSL identifier to a blank ADAE one", {
  a <- adsl0(); a$USUBJID[3] <- ""
  e <- adae0(); e$USUBJID[3] <- ""
  r <- run1(sip, ds(adsl = a, adae = e))
  expect_equal(r$findings$row, 3L)
  expect_equal(r$findings$observed, "<missing>")
})

test_that("subject_in_parent is not evaluated without the parent dataset", {
  r <- run1(sip, list(ADAE = adae0()))
  expect_equal(r$status, "not_evaluated")
})

# ---- parent_consistency ----
pc <- list(check = "parent_consistency", dataset = "ADAE",
           params = list(parent = "ADSL", variables = list(TRTA = "TRT01A")))

test_that("parent_consistency passes when carried values match", {
  expect_equal(run1(pc, ds())$status, "pass")
})

test_that("parent_consistency reports mismatches and skips orphans", {
  e <- adae0(); e$TRTA[2] <- "Arm 2"; e$USUBJID[3] <- "S-9"
  r <- run1(pc, ds(adae = e))
  expect_equal(r$findings$row, 2L)
  expect_equal(r$findings$expected, "Arm 1")
  expect_equal(r$findings$observed, "Arm 2")
})

test_that("parent_consistency treats a blank and NA as the same missing value", {
  a <- adsl0(); a$TRT01A[2] <- ""
  e <- adae0(); e$TRTA[3] <- NA
  expect_equal(run1(pc, ds(adsl = a, adae = e))$status, "pass")
})

test_that("parent_consistency is not evaluated when the parent key is duplicated", {
  a <- rbind(adsl0(), adsl0()[1, ])
  r <- run1(pc, ds(adsl = a))
  expect_equal(r$status, "not_evaluated")
  expect_match(r$detail, "not unique")
})

# ---- date_order ----
dord <- list(check = "date_order", dataset = "ADAE", params = list(start = "ASTDT", end = "AENDT"))

test_that("date_order accepts equal dates and missing end dates", {
  e <- adae0(); e$AENDT[1] <- e$ASTDT[1]; e$AENDT[2] <- NA
  expect_equal(run1(dord, ds(adae = e))$status, "pass")
})

test_that("date_order flags an end date before the start date", {
  e <- adae0(); e$AENDT[3] <- e$ASTDT[3] - 1
  r <- run1(dord, ds(adae = e))
  expect_equal(r$findings$row, 3L)
  expect_equal(r$findings$observed, "2025-01-08")
})

test_that("date_order is not evaluated on text dates", {
  e <- adae0(); e$AENDT <- format(e$AENDT)
  expect_equal(run1(dord, ds(adae = e))$status, "not_evaluated")
})

# ---- treatment_emergent ----
te <- function(window = 0, missing_start = "flag") {
  list(check = "treatment_emergent", dataset = "ADAE",
       params = list(flag = "TRTEMFL", start = "ASTDT", parent = "ADSL", trt_start = "TRTSDT",
                     trt_end = "TRTEDT", window_days = window, missing_start = missing_start))
}

test_that("treatment_emergent passes for a correctly derived flag", {
  expect_equal(run1(te(), ds())$status, "pass")
})

test_that("treatment_emergent counts first-dose day and last dose + window as emergent", {
  # S-1 doses 2025-01-01 to 2025-03-01. With a 10-day window, 2025-03-11 is the
  # last emergent day and 2025-03-12 is not.
  e <- adae0()[1:2, ]
  e$ASTDT <- as.Date(c("2025-01-01", "2025-03-11"))
  e$AENDT <- e$ASTDT
  e$TRTEMFL <- c("Y", "Y")
  expect_equal(run1(te(window = 10), ds(adae = e))$status, "pass")
  e$ASTDT[2] <- as.Date("2025-03-12")
  r <- run1(te(window = 10), ds(adae = e))
  expect_equal(r$findings$row, 2L)
  expect_equal(r$findings$expected, "<missing>")
  expect_equal(r$findings$observed, "Y")
})

test_that("treatment_emergent flags a missed flag and a pre-dose flag", {
  e <- adae0(); e$TRTEMFL <- c("", "Y", "Y")   # row 1 should be Y; row 3 is pre-dose
  r <- run1(te(), ds(adae = e))
  expect_equal(r$findings$row, c(1L, 3L))
  expect_equal(r$findings$expected, c("Y", "<missing>"))
})

test_that("treatment_emergent applies the declared missing-start convention", {
  e <- adae0(); e$ASTDT[1] <- NA
  expect_equal(run1(te(missing_start = "flag"), ds(adae = e))$status, "pass")       # flagged Y: correct
  r <- run1(te(missing_start = "do_not_flag"), ds(adae = e))
  expect_equal(r$findings$row, 1L)
})

test_that("treatment_emergent reports records it cannot derive instead of guessing", {
  a <- adsl0(); a$TRTEDT[1] <- NA
  r <- run1(te(), ds(adsl = a))
  expect_equal(r$findings$row, 1:2)
  expect_equal(unique(r$findings$observed), "cannot be derived")
})

test_that("treatment_emergent treats 'N' as disagreeing with a blank expected flag", {
  e <- adae0(); e$TRTEMFL[3] <- "N"
  r <- run1(te(), ds(adae = e))
  expect_equal(r$findings$observed, "N")
})

# ---- categorization ----
cat_rule <- list(check = "categorization", dataset = "ADSL", params = list(
  variable = "AGEGR1", source = "AGE",
  categories = list(list(label = "<40", upper = 40), list(label = "40-<60", lower = 40, upper = 60),
                    list(label = ">=60", lower = 60))))

test_that("categorization passes with lower bounds inclusive", {
  expect_equal(run1(cat_rule, ds())$status, "pass")    # 60 -> ">=60"
  a <- adsl0(); a$AGE[2] <- 40; expect_equal(run1(cat_rule, ds(adsl = a))$status, "pass")
})

test_that("categorization flags boundary errors", {
  a <- adsl0(); a$AGE[2] <- 39.99                       # belongs in <40
  r <- run1(cat_rule, ds(adsl = a))
  expect_equal(r$findings$row, 2L)
  expect_equal(r$findings$expected, "<40")
})

test_that("categorization expects a missing group when the source is missing", {
  a <- adsl0(); a$AGE[1] <- NA
  r <- run1(cat_rule, ds(adsl = a))
  expect_equal(r$findings$expected, "<missing>")
  a$AGEGR1[1] <- ""
  expect_equal(run1(cat_rule, ds(adsl = a))$status, "pass")
})

test_that("categorization reports values outside every category and overlapping cut points", {
  rule <- cat_rule; rule$params$categories <- rule$params$categories[2:3]   # nothing below 40
  r <- run1(rule, ds())
  expect_equal(r$findings$expected, "(no category covers this value)")
  rule$params$categories <- list(list(label = "a", upper = 50), list(label = "b", lower = 40))
  expect_equal(run1(rule, ds())$status, "not_evaluated")
})
