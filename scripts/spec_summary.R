#!/usr/bin/env Rscript
# Prints the two example specifications side by side. Every value is read
# from the YAML files; nothing is typed in by hand.
suppressPackageStartupMessages(
  if (requireNamespace("studygate", quietly = TRUE)) library(studygate) else pkgload::load_all(".", quiet = TRUE)
)
specs <- lapply(c("study_a.yml", "study_b.yml"), function(f) read_spec(studygate_example(f)))
pick <- function(s, check, pred = function(r) TRUE) Filter(function(r) r$check == check && pred(r), s$rules)
row <- function(s) {
  age <- pick(s, "range", function(r) r$params$variable == "AGE")[[1]]$params
  grp <- pick(s, "categorization")[[1]]$params$categories
  te <- pick(s, "treatment_emergent")[[1]]$params
  sev <- pick(s, "codelist", function(r) r$dataset == "ADAE" && r$params$codelist != "NY")[[1]]$params
  c(Study = sprintf("%s v%s", s$study$id, s$spec$version),
    Rules = length(s$rules),
    `Age range` = sprintf("%s-%s %s", age$min, age$max, tolower(s$datasets$ADSL$variables$AGE$unit %||% "")),
    `Age groups` = paste(vapply(grp, `[[`, "", "label"), collapse = " | "),
    `AE grading` = sprintf("%s: %s", sev$variable, paste(s$codelists[[sev$codelist]], collapse = ",")),
    `TEAE window` = sprintf("last dose + %s days", te$window_days))
}
`%||%` <- function(x, y) if (is.null(x)) y else x
m <- vapply(specs, row, character(6))
w <- max(nchar(m[, 1])) + 3
for (i in seq_len(nrow(m))) cat(sprintf("%-13s %-*s %s\n", rownames(m)[i], w, m[i, 1], m[i, 2]))
