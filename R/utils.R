# Internal helpers. Nothing here is exported.

`%||%` <- function(x, y) if (is.null(x)) y else x

# A value counts as missing if it is NA, or if it is a character value that is
# empty or only blanks. SAS has no separate character missing value: a blank
# character variable read from a .xpt or .sas7bdat file arrives in R as "".
# Treating NA and "" alike keeps checks identical for data from SAS or R.
sg_is_missing <- function(x) {
  if (is.character(x)) {
    is.na(x) | !nzchar(trimws(x))
  } else {
    is.na(x)
  }
}

# The type of a column, in the vocabulary a specification uses.
sg_type_of <- function(x) {
  if (inherits(x, "Date")) return("date")
  if (inherits(x, c("POSIXct", "POSIXlt"))) return("datetime")
  if (is.factor(x)) return("factor")
  if (is.logical(x)) return("logical")
  if (is.numeric(x)) return("numeric")
  if (is.character(x)) return("character")
  class(x)[1]
}

sg_type_matches <- function(x, declared) {
  identical(sg_type_of(x), declared)
}

# Raise a "this rule cannot be evaluated on these data" condition. The engine
# records the rule as NOT EVALUATED (never as passed) and keeps going.
sg_not_evaluable <- function(reason) {
  cond <- structure(
    class = c("studygate_not_evaluable", "condition"),
    list(message = reason, call = NULL)
  )
  stop(cond)
}

sg_require_columns <- function(data, cols, dataset) {
  absent <- setdiff(cols, names(data))
  if (length(absent)) {
    sg_not_evaluable(sprintf(
      "%s is missing variable(s) needed by this rule: %s",
      dataset, paste(absent, collapse = ", ")
    ))
  }
}

sg_require_type <- function(data, col, type, dataset) {
  if (!sg_type_matches(data[[col]], type)) {
    sg_not_evaluable(sprintf(
      "%s.%s must be %s to evaluate this rule but is %s",
      dataset, col, type, sg_type_of(data[[col]])
    ))
  }
}

# Format values for display in findings. Dates print as ISO 8601; missing
# values print as "<missing>" so a blank never looks like a value.
sg_fmt <- function(x) {
  if (length(x) == 0) return(character())
  out <- if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
  out[sg_is_missing(x)] <- "<missing>"
  out
}

# Build a "VAR=value; VAR2=value" record key so a programmer can find a row
# without relying on row order.
sg_record_key <- function(data, rows, key_vars) {
  key_vars <- intersect(key_vars, names(data))
  if (!length(rows)) return(character())
  if (!length(key_vars)) return(rep(NA_character_, length(rows)))
  parts <- lapply(key_vars, function(v) paste0(v, "=", sg_fmt(data[[v]][rows])))
  do.call(paste, c(parts, sep = "; "))
}

sg_empty_findings <- function() {
  data.frame(
    row = integer(), key = character(), variable = character(),
    expected = character(), observed = character(), message = character(),
    stringsAsFactors = FALSE
  )
}

sg_findings <- function(row, key, variable, expected, observed, message) {
  n <- length(row)
  if (!n) return(sg_empty_findings())
  rep_n <- function(x) if (length(x) == 1L) rep(x, n) else x
  data.frame(
    row = as.integer(row), key = rep_n(as.character(key)),
    variable = rep_n(as.character(variable)),
    expected = rep_n(as.character(expected)),
    observed = rep_n(as.character(observed)),
    message = rep_n(as.character(message)),
    stringsAsFactors = FALSE
  )
}

# Optional structured row filter from a rule: where: {variable: SAFFL, equals: "Y"}
# or where: {variable: SAFFL, in: ["Y"]}. Filters are data, never evaluated as
# code, so a specification cannot run arbitrary R.
sg_where_rows <- function(data, where, dataset) {
  if (is.null(where)) return(seq_len(nrow(data)))
  sg_require_columns(data, where$variable, dataset)
  values <- if (!is.null(where$equals)) where$equals else unlist(where[["in"]])
  keep <- !sg_is_missing(data[[where$variable]]) &
    sg_fmt(data[[where$variable]]) %in% as.character(values)
  which(keep)
}

sg_where_text <- function(where) {
  if (is.null(where)) return("")
  values <- if (!is.null(where$equals)) where$equals else unlist(where[["in"]])
  sprintf(" where %s in (%s)", where$variable,
          paste0('"', values, '"', collapse = ", "))
}

# MD5 of a data frame's content written as CSV with fixed formatting. Used as
# evidence that a report belongs to exactly these data. Column attributes such
# as SAS labels are not part of the fingerprint.
sg_data_fingerprint <- function(df) {
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  out <- as.data.frame(lapply(df, function(col) {
    if (inherits(col, "Date")) format(col, "%Y-%m-%d") else as.vector(col)
  }), stringsAsFactors = FALSE, optional = TRUE)
  names(out) <- names(df)
  utils::write.csv(out, tf, row.names = FALSE, na = "")
  unname(tools::md5sum(tf))
}

sg_text_hash <- function(text) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeLines(text, tf, useBytes = TRUE)
  unname(tools::md5sum(tf))
}
