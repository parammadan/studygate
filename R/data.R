#' Synthetic example studies
#'
#' Hand-written, **synthetic** ADSL/ADAE-style datasets. They contain no real
#' patient information; identifiers, dates, regimens and values are invented.
#' `data-raw/make_examples.R` builds them and documents every intentional
#' defect.
#'
#' * `study_a`: adult HIV treatment study (10 subjects, 12 AEs). It has one
#'   deliberate defect: an ADAE record for subject `SYN-A-011`, who is not in
#'   ADSL (a transposition of `SYN-A-001`).
#' * `study_b`: pediatric HIV study (8 subjects, 8 AEs, age in months). Every
#'   Study B rule passes.
#' * `study_a_defects`: Study A with one documented defect per rule type, used
#'   in tests and the vignette.
#'
#' @format Each is a named list with two data frames, `ADSL` and `ADAE`.
#' @source Synthetic; see `data-raw/make_examples.R`.
#' @name example_studies
#' @aliases study_a study_b study_a_defects
#' @keywords datasets
"study_a"

#' @rdname example_studies
"study_b"

#' @rdname example_studies
"study_a_defects"
