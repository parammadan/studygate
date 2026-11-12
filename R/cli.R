#' Command-line interface
#'
#' Used by the `bin/studygate` script so the package can run in Bash
#' pipelines and batch jobs on Linux servers.
#'
#' ```
#' studygate validate --spec study_a.yml --data path/to/datasets [--out report_dir]
#' studygate explain  --spec study_a.yml --rule SGA-ADAE-004
#' studygate rules    --spec study_a.yml
#' studygate checks
#' ```
#'
#' Exit codes: 0 = gate PASS (or command succeeded), 2 = gate BLOCKED,
#' 1 = usage, specification or input error.
#'
#' @param args Character vector of arguments.
#' @return The exit code (integer), invisibly.
#' @export
studygate_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  usage <- paste(
    "usage:",
    "  studygate validate --spec FILE --data DIR [--out DIR] [--max N]",
    "  studygate explain  --spec FILE --rule ID",
    "  studygate rules    --spec FILE",
    "  studygate checks", sep = "\n")
  fail <- function(msg) {
    cat(msg, "\n", sep = "", file = stderr())
    invisible(1L)
  }
  if (!length(args) || args[1] %in% c("-h", "--help", "help")) {
    cat(usage, "\n")
    return(invisible(if (length(args)) 0L else 1L))
  }
  cmd <- args[1]
  opts <- sg_parse_opts(args[-1])
  if (is.character(opts)) return(fail(paste0(opts, "\n", usage)))
  need <- function(...) {
    miss <- setdiff(c(...), names(opts))
    if (length(miss)) paste0("missing option(s): ", paste0("--", miss, collapse = " ")) else NULL
  }
  tryCatch({
    if (cmd == "checks") {
      ch <- list_checks()
      for (i in seq_len(nrow(ch))) cat(sprintf("%-20s %s\n", ch$check[i], ch$description[i]))
      return(invisible(0L))
    }
    if (cmd == "rules") {
      if (!is.null(m <- need("spec"))) return(fail(m))
      spec <- read_spec(opts$spec)
      print(spec)
      r <- list_rules(spec)
      for (i in seq_len(nrow(r))) {
        cat(sprintf("  %-14s %-5s %-19s %-8s %s\n", r$id[i], r$dataset[i], r$check[i], r$severity[i], r$title[i]))
      }
      return(invisible(0L))
    }
    if (cmd == "explain") {
      if (!is.null(m <- need("spec", "rule"))) return(fail(m))
      explain_rule(opts$spec, opts$rule)
      return(invisible(0L))
    }
    if (cmd == "validate") {
      if (!is.null(m <- need("spec", "data"))) return(fail(m))
      spec <- read_spec(opts$spec)
      datasets <- read_study_datasets(opts$data, spec)
      if (!length(datasets)) return(fail(sprintf("no dataset files for %s found in %s",
                                                 paste(names(spec$datasets), collapse = "/"), opts$data)))
      res <- validate_study(datasets, spec)
      report_validation(res, max_findings = as.integer(opts$max %||% 10))
      if (!is.null(opts$out)) {
        paths <- write_validation_report(res, opts$out)
        cat(sprintf("Report written to %s/ (%s)\n", opts$out, paste(basename(paths), collapse = ", ")))
      }
      return(invisible(if (gate_passed(res)) 0L else 2L))
    }
    fail(paste0("unknown command: ", cmd, "\n", usage))
  }, studygate_spec_error = function(e) fail(conditionMessage(e)),
     error = function(e) fail(paste("error:", conditionMessage(e))))
}

sg_parse_opts <- function(args) {
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    if (!startsWith(a, "--")) return(sprintf("unexpected argument: %s", a))
    if (i == length(args) || startsWith(args[i + 1], "--")) return(sprintf("option %s needs a value", a))
    opts[[sub("^--", "", a)]] <- args[i + 1]
    i <- i + 2
  }
  opts
}
