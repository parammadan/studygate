# Build a minimal in-memory specification for one rule so each test states
# its own inputs and expectations, independent of the example studies.
mk_spec <- function(rules, datasets = NULL, codelists = list(NY = c("Y", "N"))) {
  if (is.null(datasets)) {
    datasets <- list(
      ADSL = list(key = list("USUBJID"), variables = list(
        USUBJID = list(type = "character", required = TRUE),
        AGE = list(type = "numeric", required = TRUE),
        AGEGR1 = list(type = "character"),
        SEX = list(type = "character", required = TRUE),
        SAFFL = list(type = "character"),
        TRT01A = list(type = "character"),
        TRTSDT = list(type = "date"),
        TRTEDT = list(type = "date"))),
      ADAE = list(key = list("USUBJID", "AESEQ"), variables = list(
        USUBJID = list(type = "character", required = TRUE),
        AESEQ = list(type = "numeric", required = TRUE),
        AESEV = list(type = "character"),
        ASTDT = list(type = "date"),
        AENDT = list(type = "date"),
        TRTA = list(type = "character"),
        TRTEMFL = list(type = "character")))
    )
  }
  rules <- lapply(seq_along(rules), function(i) {
    r <- rules[[i]]
    r$id <- r$id %||% sprintf("T-%03d", i)
    r$severity <- r$severity %||% "error"
    r$why <- r$why %||% "test why"
    r$guidance <- r$guidance %||% "test guidance"
    r$reference <- r$reference %||% "test reference"
    r
  })
  list(schema_version = 1L, study = list(id = "TEST"), spec = list(version = "0.0.1"),
       codelists = codelists, datasets = datasets, rules = rules)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Run one rule and return list(status, findings) for that rule.
run1 <- function(rule, datasets, ...) {
  res <- validate_study(datasets, mk_spec(list(rule), ...))
  list(status = res$rules$status[1], detail = res$rules$detail[1],
       findings = res$findings, result = res)
}

adsl0 <- function() {
  data.frame(
    USUBJID = c("S-1", "S-2", "S-3"),
    AGE = c(30, 45, 60),
    AGEGR1 = c("<40", "40-<60", ">=60"),
    SEX = c("M", "F", "F"),
    SAFFL = c("Y", "Y", "N"),
    TRT01A = c("Arm 1", "Arm 2", ""),
    TRTSDT = as.Date(c("2025-01-01", "2025-01-10", NA)),
    TRTEDT = as.Date(c("2025-03-01", "2025-03-10", NA)),
    stringsAsFactors = FALSE
  )
}

adae0 <- function() {
  data.frame(
    USUBJID = c("S-1", "S-1", "S-2"),
    AESEQ = c(1, 2, 1),
    AESEV = c("MILD", "SEVERE", "MODERATE"),
    ASTDT = as.Date(c("2025-01-05", "2025-02-01", "2025-01-09")),
    AENDT = as.Date(c("2025-01-06", "2025-02-03", "2025-01-12")),
    TRTA = c("Arm 1", "Arm 1", "Arm 2"),
    TRTEMFL = c("Y", "Y", ""),
    stringsAsFactors = FALSE
  )
}
