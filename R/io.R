#' Read a folder of study datasets
#'
#' Reads one file per dataset from `dir`, named after the dataset in lower or
#' upper case (`adsl.xpt`, `ADAE.sas7bdat`, `adsl.rds`, `adae.csv`). SAS
#' transport (`.xpt`) and SAS data set (`.sas7bdat`) files need the haven
#' package.
#'
#' CSV files are read as text and then typed using the specification. A
#' column is converted to numeric or date only if **every** non-blank value
#' converts cleanly (dates must be ISO 8601 `YYYY-MM-DD`). Otherwise the column
#' is left as text, and the `variable_types` check reports it. Nothing is
#' coerced to missing silently.
#'
#' @param dir A directory.
#' @param specification A specification. Only its datasets are read.
#' @return A named list of data frames, ready for [validate_study()].
#' @examples
#' dir <- system.file("extdata", "studies", "study_b", package = "studygate")
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   ds <- read_study_datasets(dir, studygate_example("study_b.yml"))
#'   names(ds)
#' }
#' @export
read_study_datasets <- function(dir, specification) {
  spec <- read_spec(specification)
  if (!dir.exists(dir)) stop("data directory not found: ", dir, call. = FALSE)
  files <- list.files(dir, full.names = TRUE)
  stem <- toupper(tools::file_path_sans_ext(basename(files)))
  ext <- tolower(tools::file_ext(files))
  out <- list()
  for (ds in names(spec$datasets)) {
    hit <- files[stem == toupper(ds) & ext %in% c("xpt", "sas7bdat", "rds", "csv")]
    if (!length(hit)) next
    if (length(hit) > 1) {
      stop(sprintf("more than one file for %s in %s: %s", ds, dir, paste(basename(hit), collapse = ", ")),
           call. = FALSE)
    }
    out[[ds]] <- sg_read_one(hit, spec$datasets[[ds]])
  }
  out
}

sg_read_one <- function(path, ds_spec) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xpt", "sas7bdat")) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("reading .", ext, " files needs the haven package: install.packages(\"haven\")", call. = FALSE)
    }
    d <- if (ext == "xpt") haven::read_xpt(path) else haven::read_sas(path)
    d <- as.data.frame(haven::zap_labels(haven::zap_formats(d)), stringsAsFactors = FALSE)
    return(d)
  }
  if (ext == "rds") return(as.data.frame(readRDS(path), stringsAsFactors = FALSE))
  d <- utils::read.csv(path, colClasses = "character", na.strings = character(), check.names = FALSE)
  for (v in intersect(names(ds_spec$variables), names(d))) {
    type <- ds_spec$variables[[v]]$type
    x <- d[[v]]
    blank <- !nzchar(trimws(x))
    if (type == "numeric") {
      num <- suppressWarnings(as.numeric(x))
      if (!any(is.na(num) & !blank)) d[[v]] <- num
    } else if (type == "date") {
      ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) | blank
      dt <- as.Date(ifelse(blank, NA, x), format = "%Y-%m-%d")
      if (all(ok) && !any(is.na(dt) & !blank)) d[[v]] <- dt
    }
  }
  d
}
