#' Print a full validation report to the console
#'
#' Lists every rule with its status. For each rule that did not pass, it shows
#' the affected records (expected versus observed), why the rule matters, what
#' to investigate, and the rule's reference.
#'
#' @param result A `studygate_result` from [validate_study()].
#' @param max_findings Maximum findings printed per rule (all are kept in the
#'   result and in exported reports).
#' @return `result`, invisibly.
#' @examples
#' res <- validate_study(study_a, studygate_example("study_a.yml"))
#' report_validation(res)
#' @export
report_validation <- function(result, max_findings = 10) {
  stopifnot(inherits(result, "studygate_result"))
  r <- result$rules
  line <- strrep("-", 78)
  cat(line, "\n", sep = "")
  cat(sprintf("StudyGate validation report: %s (%s)\n", result$study$id, result$study$title %||% ""))
  cat(sprintf("Specification v%s  md5 %s\n", result$spec$version, result$spec$md5))
  cat(sprintf("Evidence id %s  |  studygate %s  |  %s\n", result$evidence$evidence_id,
              result$evidence$package_version, result$evidence$run_time))
  if (isTRUE(result$study$synthetic)) cat("Data: SYNTHETIC example data, no real patients\n")
  cat(line, "\n", sep = "")
  icon <- c(pass = "PASS", fail = "FAIL", not_evaluated = "N/E ", error = "ERR ")
  for (i in seq_len(nrow(r))) {
    cat(sprintf("[%s] %-14s %-5s %-8s %s%s\n", icon[[r$status[i]]], r$rule_id[i], r$dataset[i],
                r$severity[i], r$title[i], if (r$status[i] == "pass") "" else paste0("  (", r$detail[i], ")")))
  }
  todo <- r[r$status != "pass", , drop = FALSE]
  for (i in seq_len(nrow(todo))) {
    rule <- result$specification$rules[[todo$rule_id[i]]]
    f <- result$findings[result$findings$rule_id == todo$rule_id[i], , drop = FALSE]
    cat("\n", line, "\n", sep = "")
    cat(sprintf("%s  %s  [%s%s]\n", rule$id, rule$title, rule$severity,
                if (isTRUE(rule$blocking)) ", blocks release" else ", does not block release"))
    if (todo$status[i] != "fail") {
      cat(sprintf("  Status: %s. %s\n", toupper(gsub("_", " ", todo$status[i])), todo$detail[i]))
      cat("  A rule that cannot run is not a pass. Make the data support the rule, then rerun.\n")
    }
    shown <- utils::head(f, max_findings)
    for (j in seq_len(nrow(shown))) {
      where <- if (is.na(shown$row[j])) shown$dataset[j] else
        sprintf("%s row %d (%s)", shown$dataset[j], shown$row[j], shown$key[j])
      cat(sprintf("  * %s\n      %s | expected: %s | observed: %s\n",
                  where, shown$message[j], shown$expected[j], shown$observed[j]))
    }
    if (nrow(f) > max_findings) cat(sprintf("  ... %d more in the exported report\n", nrow(f) - max_findings))
    cat(sg_wrap("Why it matters: ", rule$why))
    cat(sg_wrap("Investigate: ", rule$guidance))
    cat(sprintf("  Reference: %s\n", rule$reference))
    cat(sprintf("  See the rows in R: %s\n", gsub("\n", "\n    ", sg_checks[[rule$check]]$r_code(rule, result$specification))))
  }
  cat("\n", line, "\n", sep = "")
  n_block <- sum(r$blocking & r$status != "pass")
  cat(sprintf("GATE: %s  (%d blocking rule%s not passed, %d finding%s in total)\n", result$gate,
              n_block, if (n_block != 1) "s" else "", nrow(result$findings),
              if (nrow(result$findings) != 1) "s" else ""))
  invisible(result)
}

sg_wrap <- function(label, text) {
  body <- strwrap(text, width = 74, initial = paste0("  ", label), prefix = "    ")
  paste0(paste(body, collapse = "\n"), "\n")
}

#' Explain a rule for a programmer
#'
#' Prints what a rule checks, why it matters, how to investigate a failure,
#' where the requirement comes from, R code that shows the affected rows, and
#' an illustrative SAS analogue for programmers who know SAS.
#'
#' @param x A specification (path, list, `studygate_spec`) or a
#'   `studygate_result`.
#' @param rule_id The rule identifier, e.g. `"SGA-ADAE-004"`.
#' @return The rule (a list), invisibly.
#' @examples
#' explain_rule(studygate_example("study_a.yml"), "SGA-ADAE-004")
#' @export
explain_rule <- function(x, rule_id) {
  spec <- if (inherits(x, "studygate_result")) x$specification else read_spec(x)
  rule <- spec$rules[[rule_id]]
  if (is.null(rule)) {
    stop(sprintf("no rule '%s' in %s v%s. Rules: %s", rule_id, spec$study$id, spec$spec$version,
                 paste(names(spec$rules), collapse = ", ")), call. = FALSE)
  }
  def <- sg_checks[[rule$check]]
  cat(sprintf("%s  %s\n", rule$id, rule$title))
  cat(sprintf("  Study %s, specification v%s%s\n", spec$study$id, spec$spec$version,
              if (!is.null(rule$since)) sprintf(" (rule added in v%s)", rule$since) else ""))
  cat(sprintf("  Dataset: %s   Check: %s   Severity: %s   Blocks release: %s\n",
              rule$dataset, rule$check, rule$severity, if (isTRUE(rule$blocking)) "yes" else "no"))
  cat(sg_wrap("What it checks: ", def$description))
  if (length(rule$params)) {
    cat("  Parameters:\n")
    for (n in names(rule$params)) {
      val <- rule$params[[n]]
      txt <- if (is.list(val)) gsub("\n", " ", trimws(yaml::as.yaml(val))) else paste(val, collapse = ", ")
      cat(sprintf("    %s: %s\n", n, txt))
    }
  }
  cat(sg_wrap("Why it matters: ", rule$why))
  cat(sg_wrap("Investigate: ", rule$guidance))
  cat(sprintf("  Reference: %s\n", rule$reference))
  cat(sprintf("  R, to see the rows:\n    %s\n", gsub("\n", "\n    ", def$r_code(rule, spec))))
  cat(sprintf("  SAS analogue (illustrative, not executed by studygate):\n    %s\n", def$sas_code(rule, spec)))
  invisible(rule)
}

#' Export a validation report
#'
#' Writes four files that together form the evidence for one validation run:
#' `report.html` (human-readable), `findings.csv`, `rules.csv`, and
#' `evidence.json` (specification hash, data fingerprints, versions, gate).
#'
#' @param result A `studygate_result`.
#' @param dir Output directory, created if needed.
#' @return The paths of the written files, invisibly.
#' @examples
#' res <- validate_study(study_b, studygate_example("study_b.yml"))
#' out <- write_validation_report(res, tempfile("studygate-"))
#' basename(out)
#' @export
write_validation_report <- function(result, dir) {
  stopifnot(inherits(result, "studygate_result"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    html = file.path(dir, "report.html"),
    findings = file.path(dir, "findings.csv"),
    rules = file.path(dir, "rules.csv"),
    evidence = file.path(dir, "evidence.json")
  )
  utils::write.csv(result$findings, paths[["findings"]], row.names = FALSE, na = "")
  utils::write.csv(result$rules, paths[["rules"]], row.names = FALSE, na = "")
  ev <- c(list(study_id = result$study$id, spec_version = result$spec$version, gate = result$gate,
               rule_status = as.list(structure(result$rules$status, names = result$rules$rule_id)),
               n_findings = nrow(result$findings),
               synthetic_data = isTRUE(result$study$synthetic)),
          result$evidence)
  jsonlite::write_json(ev, paths[["evidence"]], auto_unbox = TRUE, pretty = TRUE)
  writeLines(sg_html(result), paths[["html"]], useBytes = TRUE)
  invisible(paths)
}

sg_esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

sg_html <- function(result) {
  r <- result$rules
  f <- result$findings
  spec <- result$specification
  gate_class <- if (result$gate == "PASS") "pass" else "block"
  rule_rows <- paste0(sprintf(
    "<tr class='%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    r$status, sg_esc(r$rule_id), sg_esc(r$dataset), sg_esc(r$title), sg_esc(r$severity),
    ifelse(r$blocking, "yes", "no"), sg_esc(toupper(gsub("_", " ", r$status))), sg_esc(r$detail)
  ), collapse = "\n")
  details <- vapply(r$rule_id[r$status != "pass"], function(id) {
    rule <- spec$rules[[id]]
    ff <- f[f$rule_id == id, , drop = FALSE]
    rows <- if (nrow(ff)) paste0(sprintf(
      "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
      ifelse(is.na(ff$row), "", ff$row), sg_esc(ifelse(is.na(ff$key), "", ff$key)), sg_esc(ff$variable),
      sg_esc(ff$expected), sg_esc(ff$observed), sg_esc(ff$message)), collapse = "\n") else ""
    status <- r$status[r$rule_id == id]
    sprintf(paste0(
      "<section class='rule'><h3>%s: %s</h3><p class='meta'>%s | severity %s | %s</p>%s",
      "%s<p><b>Why it matters.</b> %s</p><p><b>Investigate.</b> %s</p><p><b>Reference.</b> %s</p>",
      "<p><b>See the rows in R:</b></p><pre>%s</pre></section>"),
      sg_esc(id), sg_esc(rule$title), sg_esc(rule$dataset), sg_esc(rule$severity),
      if (isTRUE(rule$blocking)) "blocks release" else "does not block release",
      if (status != "fail") sprintf("<p class='ne'><b>%s.</b> %s A rule that cannot run is not a pass.</p>",
                                    sg_esc(toupper(gsub("_", " ", status))), sg_esc(r$detail[r$rule_id == id])) else "",
      if (nrow(ff)) paste0("<table><tr><th>Row</th><th>Record</th><th>Variable</th><th>Expected</th>",
                           "<th>Observed</th><th>Finding</th></tr>", rows, "</table>") else "",
      sg_esc(rule$why), sg_esc(rule$guidance), sg_esc(rule$reference),
      sg_esc(sg_checks[[rule$check]]$r_code(rule, spec)))
  }, "")
  ev <- result$evidence
  ev_rows <- paste0(sprintf("<tr><td>%s</td><td><code>%s</code></td></tr>",
                            c("Evidence id", "Specification", "Specification md5",
                              paste("Data md5:", names(ev$data_md5)), "studygate", "R", "Run time"),
                            sg_esc(c(ev$evidence_id, ev$spec_source, ev$spec_md5, unlist(ev$data_md5),
                                     ev$package_version, ev$r_version, ev$run_time))), collapse = "\n")
  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>StudyGate report ", sg_esc(result$study$id), "</title><style>",
    "body{font:15px/1.5 system-ui,-apple-system,Segoe UI,sans-serif;margin:0 auto;max-width:1100px;padding:24px 16px;color:#1d2433;background:#fff}",
    "h1{font-size:22px;margin:0}h2{font-size:17px;margin-top:32px}h3{font-size:15px;margin:0 0 4px}",
    ".gate{display:inline-block;padding:6px 14px;border-radius:6px;font-weight:700;margin:12px 0}",
    ".gate.pass{background:#e3f6e8;color:#14532d}.gate.block{background:#fde8e8;color:#7f1d1d}",
    "table{border-collapse:collapse;width:100%;font-size:13px;margin:8px 0}th,td{border:1px solid #d9dee8;padding:5px 8px;text-align:left;vertical-align:top}",
    "th{background:#f3f5f9}td:first-child{white-space:nowrap}tr.fail td{background:#fff5f5}tr.not_evaluated td,tr.error td{background:#fffbea}",
    ".rule{border:1px solid #d9dee8;border-radius:8px;padding:14px;margin:14px 0}.meta{color:#5b6475;margin:0 0 8px}",
    ".ne{background:#fffbea;padding:6px 10px;border-radius:4px}pre{background:#f3f5f9;padding:8px;overflow-x:auto;font-size:12.5px}",
    ".note{color:#5b6475;font-size:13px}</style></head><body>",
    "<h1>StudyGate validation report: ", sg_esc(result$study$id), "</h1>",
    "<p>", sg_esc(result$study$title %||% ""), " | specification v", sg_esc(result$spec$version), "</p>",
    if (isTRUE(result$study$synthetic)) "<p class='note'>Synthetic example data. No real patient information.</p>" else "",
    "<div class='gate ", gate_class, "'>GATE: ", result$gate, "</div>",
    "<h2>Rules</h2><table><tr><th>Rule</th><th>Dataset</th><th>Requirement</th><th>Severity</th>",
    "<th>Blocks</th><th>Status</th><th>Detail</th></tr>", rule_rows, "</table>",
    if (length(details)) paste0("<h2 id='findings'>Findings and guidance</h2>", paste(details, collapse = "\n")) else
      "<h2>Findings</h2><p>No findings. Every rule ran and passed.</p>",
    "<h2>Evidence</h2><table>", ev_rows, "</table>",
    "<p class='note'>Generated by studygate, an independent research prototype. Not a validated or ",
    "regulatory-certified tool and not a CDISC conformance check.</p></body></html>"
  )
}
