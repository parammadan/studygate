#' Validate analysis datasets against a study specification
#'
#' Runs every rule in the specification against the supplied datasets and
#' returns a structured, reproducible result. Data are never modified.
#'
#' Each rule ends in exactly one status:
#' * `pass`: the rule ran and found nothing.
#' * `fail`: the rule ran and found one or more records that break it.
#' * `not_evaluated`: the data cannot support the rule (for example a variable
#'   it needs is absent or has the wrong type). This is **not** a pass.
#' * `error`: the rule crashed unexpectedly. This is **not** a pass either.
#'
#' The release gate is `BLOCKED` when any blocking rule ends in `fail`,
#' `not_evaluated` or `error`; otherwise it is `PASS`.
#'
#' @param datasets A named list of data frames, such as
#'   `list(ADSL = adsl, ADAE = adae)`. Names are matched to the
#'   specification's dataset names without regard to case. Tibbles and data
#'   read with haven are accepted.
#' @param specification A YAML path, a list, or a `studygate_spec` from
#'   [read_spec()].
#' @param run_time Time stamp recorded in the evidence. It does not affect
#'   findings or the evidence id.
#' @return An object of class `studygate_result`: a list with `study`,
#'   `spec`, `gate`, `rules` (one row per rule), `findings` (one row per
#'   finding) and `evidence` (hashes and versions).
#' @examples
#' res <- validate_study(study_a, studygate_example("study_a.yml"))
#' res
#' @export
validate_study <- function(datasets, specification, run_time = Sys.time()) {
  spec <- read_spec(specification)
  datasets <- sg_prepare_datasets(datasets, spec)

  rule_rows <- vector("list", length(spec$rules))
  finding_rows <- vector("list", length(spec$rules))
  for (i in seq_along(spec$rules)) {
    out <- sg_run_rule(spec$rules[[i]], datasets, spec)
    rule_rows[[i]] <- out$rule
    finding_rows[[i]] <- out$findings
  }
  rules <- do.call(rbind, rule_rows)
  findings <- do.call(rbind, finding_rows)
  rownames(rules) <- NULL
  rownames(findings) <- NULL

  blocked <- rules$blocking & rules$status %in% c("fail", "not_evaluated", "error")
  gate <- if (any(blocked)) "BLOCKED" else "PASS"

  data_hashes <- vapply(datasets, sg_data_fingerprint, "")
  evidence <- list(
    spec_source = spec$source,
    spec_md5 = spec$hash,
    data_md5 = as.list(data_hashes),
    data_rows = as.list(vapply(datasets, nrow, integer(1))),
    package_version = as.character(utils::packageVersion("studygate")),
    r_version = R.version.string,
    run_time = format(run_time, "%Y-%m-%dT%H:%M:%S%z")
  )
  # Same spec + same data + same package version -> same evidence id.
  evidence$evidence_id <- substr(sg_text_hash(c(
    spec$hash, paste(names(data_hashes), data_hashes), evidence$package_version
  )), 1, 12)

  structure(
    list(
      study = spec$study,
      spec = list(version = spec$spec$version, effective_date = spec$spec$effective_date %||% NA,
                  source = spec$source, md5 = spec$hash),
      gate = gate,
      rules = rules,
      findings = findings,
      evidence = evidence,
      specification = spec
    ),
    class = "studygate_result"
  )
}

sg_prepare_datasets <- function(datasets, spec) {
  if (is.data.frame(datasets) || !is.list(datasets) || is.null(names(datasets)) ||
      any(!nzchar(names(datasets)))) {
    stop(structure(
      class = c("studygate_input_error", "error", "condition"),
      list(message = "datasets must be a named list of data frames, e.g. list(ADSL = adsl, ADAE = adae)",
           call = NULL)
    ))
  }
  not_df <- names(datasets)[!vapply(datasets, is.data.frame, logical(1))]
  if (length(not_df)) {
    stop(structure(
      class = c("studygate_input_error", "error", "condition"),
      list(message = sprintf("not a data frame: %s", paste(not_df, collapse = ", ")), call = NULL)
    ))
  }
  up <- toupper(names(datasets))
  if (anyDuplicated(up)) {
    stop(structure(
      class = c("studygate_input_error", "error", "condition"),
      list(message = "dataset names must be unique ignoring case", call = NULL)
    ))
  }
  spec_names <- names(spec$datasets)
  matched <- spec_names[match(up, toupper(spec_names))]
  names(datasets) <- ifelse(is.na(matched), names(datasets), matched)
  # Work on plain data frames; the caller's objects are untouched.
  lapply(datasets, function(d) as.data.frame(d, stringsAsFactors = FALSE))
}

sg_run_rule <- function(rule, datasets, spec) {
  def <- sg_checks[[rule$check]]
  data <- datasets[[rule$dataset]]
  status <- "pass"
  detail <- ""
  findings <- sg_empty_findings()

  if (is.null(data)) {
    status <- "not_evaluated"
    detail <- sprintf("dataset %s was not supplied", rule$dataset)
  } else {
    ctx <- list(
      data = data, datasets = datasets, rule = rule, params = rule$params,
      spec = spec, dataset = rule$dataset,
      key = unlist(spec$datasets[[rule$dataset]]$key)
    )
    res <- tryCatch(
      def$run(ctx),
      studygate_not_evaluable = function(e) structure(conditionMessage(e), class = "sg_ne"),
      error = function(e) structure(conditionMessage(e), class = "sg_err")
    )
    if (inherits(res, "sg_ne")) {
      status <- "not_evaluated"; detail <- unclass(res)
    } else if (inherits(res, "sg_err")) {
      status <- "error"; detail <- paste("unexpected error:", unclass(res))
    } else if (nrow(res)) {
      status <- "fail"
      findings <- res[order(res$row, res$variable, na.last = TRUE), , drop = FALSE]
      detail <- sprintf("%d finding%s", nrow(findings), if (nrow(findings) != 1) "s" else "")
    }
  }

  rule_row <- data.frame(
    rule_id = rule$id, dataset = rule$dataset, check = rule$check, title = rule$title,
    severity = rule$severity, blocking = isTRUE(rule$blocking), status = status,
    n_findings = nrow(findings), detail = detail,
    stringsAsFactors = FALSE
  )
  if (nrow(findings)) {
    findings <- cbind(
      data.frame(rule_id = rule$id, dataset = rule$dataset, severity = rule$severity,
                 blocking = isTRUE(rule$blocking), stringsAsFactors = FALSE),
      findings,
      data.frame(why = rule$why, guidance = rule$guidance, reference = rule$reference,
                 stringsAsFactors = FALSE)
    )
  } else {
    findings <- cbind(
      data.frame(rule_id = character(), dataset = character(), severity = character(),
                 blocking = logical(), stringsAsFactors = FALSE),
      findings,
      data.frame(why = character(), guidance = character(), reference = character(),
                 stringsAsFactors = FALSE)
    )
  }
  list(rule = rule_row, findings = findings)
}

#' @export
print.studygate_result <- function(x, ...) {
  s <- table(factor(x$rules$status, c("pass", "fail", "not_evaluated", "error")))
  cat(sprintf("StudyGate | %s | spec v%s | evidence %s\n", x$study$id, x$spec$version, x$evidence$evidence_id))
  cat(sprintf("Rules: %d  pass %d  fail %d  not evaluated %d  error %d  | findings: %d\n",
              nrow(x$rules), s[["pass"]], s[["fail"]], s[["not_evaluated"]], s[["error"]], nrow(x$findings)))
  cat(sprintf("GATE: %s\n", x$gate))
  invisible(x)
}

#' @export
as.data.frame.studygate_result <- function(x, ...) x$findings

#' Has a validation run passed its release gate?
#'
#' @param result A `studygate_result`.
#' @return `TRUE` when no blocking rule failed, was not evaluated, or errored.
#' @examples
#' gate_passed(validate_study(study_b, studygate_example("study_b.yml")))
#' @export
gate_passed <- function(result) {
  stopifnot(inherits(result, "studygate_result"))
  identical(result$gate, "PASS")
}

#' Compare two validation runs
#'
#' Shows which findings were resolved, which are new, and which persist
#' between a run before and after a correction. Findings are matched on rule,
#' dataset, record key and variable, not on row number, so re-sorting a
#' dataset does not create false differences.
#'
#' @param before,after `studygate_result` objects.
#' @return A data frame of findings with a `change` column
#'   (`resolved`, `new`, `persisting`).
#' @examples
#' bad <- validate_study(study_a, studygate_example("study_a.yml"))
#' fixed <- study_a
#' fixed$ADAE$USUBJID[fixed$ADAE$USUBJID == "SYN-A-011"] <- "SYN-A-001"
#' compare_results(bad, validate_study(fixed, studygate_example("study_a.yml")))
#' @export
compare_results <- function(before, after) {
  stopifnot(inherits(before, "studygate_result"), inherits(after, "studygate_result"))
  sig <- function(f) paste(f$rule_id, f$dataset, f$key, f$variable, f$expected, sep = "|")
  b <- before$findings; a <- after$findings
  sb <- sig(b); sa <- sig(a)
  cols <- c("rule_id", "dataset", "key", "variable", "expected", "observed", "severity")
  out <- rbind(
    if (nrow(b)) cbind(change = ifelse(sb %in% sa, "persisting", "resolved"), b[cols]),
    if (nrow(a)) cbind(change = rep("new", sum(!sa %in% sb)), a[!sa %in% sb, cols, drop = FALSE])
  )
  if (is.null(out)) {
    out <- data.frame(change = character(), rule_id = character(), dataset = character(),
                      key = character(), variable = character(), expected = character(),
                      observed = character(), severity = character(), stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}
