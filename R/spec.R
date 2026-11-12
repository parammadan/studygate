#' Read and check a study specification
#'
#' A specification is a versioned YAML file (or an equivalent R list) that
#' declares a study's datasets, variables, controlled terminology and
#' validation rules. `read_spec()` checks the whole file before any data are
#' touched and reports **every** problem at once, so a typo in a rule is found
#' when the specification is written, not halfway through a validation run.
#'
#' @param specification Path to a YAML file, or a list with the same structure.
#' @return An object of class `studygate_spec`.
#' @section Errors:
#' An invalid specification raises an error of class `studygate_spec_error`
#' whose `problems` field lists each problem as a character string.
#' @examples
#' spec <- read_spec(studygate_example("study_a.yml"))
#' spec
#' @export
read_spec <- function(specification) {
  if (inherits(specification, "studygate_spec")) return(specification)
  if (is.character(specification) && length(specification) == 1L) {
    if (!file.exists(specification)) {
      sg_abort_spec(sprintf("specification file not found: %s", specification))
    }
    text <- readLines(specification, warn = FALSE, encoding = "UTF-8")
    raw <- tryCatch(
      yaml::yaml.load(paste(text, collapse = "\n")),
      error = function(e) sg_abort_spec(paste("YAML could not be parsed:", conditionMessage(e)))
    )
    source <- specification
    hash <- unname(tools::md5sum(specification))
  } else if (is.list(specification)) {
    raw <- specification
    source <- "<in-memory list>"
    hash <- sg_text_hash(yaml::as.yaml(specification))
  } else {
    sg_abort_spec("specification must be a file path or a list")
  }

  problems <- sg_spec_problems(raw)
  if (length(problems)) sg_abort_spec(problems)

  rules <- lapply(raw$rules, function(r) {
    r$params <- r$params %||% list()
    r$blocking <- r$blocking %||% identical(r$severity, "error")
    r$title <- r$title %||% r$id
    r
  })
  names(rules) <- vapply(rules, `[[`, "", "id")

  structure(
    list(
      schema_version = raw$schema_version,
      study = raw$study,
      spec = raw$spec,
      codelists = lapply(raw$codelists %||% list(), as.character),
      datasets = raw$datasets,
      rules = rules,
      source = source,
      hash = hash
    ),
    class = "studygate_spec"
  )
}

sg_abort_spec <- function(problems) {
  msg <- paste0(
    "Invalid StudyGate specification (", length(problems), " problem",
    if (length(problems) != 1) "s", "):\n",
    paste0("  - ", problems, collapse = "\n")
  )
  stop(structure(
    class = c("studygate_spec_error", "error", "condition"),
    list(message = msg, call = NULL, problems = problems)
  ))
}

sg_valid_types <- c("character", "numeric", "date")
sg_valid_severities <- c("error", "warning", "note")

sg_is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))

# Returns a character vector of problems (empty when the spec is valid).
sg_spec_problems <- function(raw) {
  p <- character()
  add <- function(...) p <<- c(p, sprintf(...))

  if (!is.list(raw)) return("top level must be a mapping")
  if (!identical(as.integer(raw$schema_version %||% NA), 1L)) {
    add("schema_version must be 1")
  }
  if (!sg_is_string(raw$study$id)) add("study.id is required")
  if (!sg_is_string(raw$spec$version)) add("spec.version is required")
  if (!is.null(raw$spec$version) &&
      !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", as.character(raw$spec$version))) {
    add("spec.version must look like MAJOR.MINOR.PATCH, got '%s'", raw$spec$version)
  }

  codelists <- raw$codelists %||% list()
  for (cl in names(codelists)) {
    vals <- codelists[[cl]]
    if (!length(vals)) add("codelists.%s is empty", cl)
    if (any(vapply(vals, is.logical, logical(1)))) {
      # YAML 1.1 reads unquoted Y, N, yes, no, on, off as true/false.
      add("codelists.%s contains true/false: quote coded values such as 'Y' and 'N' in YAML", cl)
    }
  }
  datasets <- raw$datasets
  if (!is.list(datasets) || !length(datasets)) {
    add("datasets must declare at least one dataset")
    datasets <- list()
  }
  for (ds in names(datasets)) {
    vars <- datasets[[ds]]$variables
    if (!is.list(vars) || !length(vars)) {
      add("datasets.%s.variables must declare at least one variable", ds)
      next
    }
    for (v in names(vars)) {
      type <- vars[[v]]$type
      if (!sg_is_string(type) || !type %in% sg_valid_types) {
        add("datasets.%s.variables.%s.type must be one of %s", ds, v,
            paste(sg_valid_types, collapse = ", "))
      }
    }
    key <- unlist(datasets[[ds]]$key)
    bad_key <- setdiff(key, names(vars))
    if (length(bad_key)) {
      add("datasets.%s.key uses undeclared variable(s): %s", ds, paste(bad_key, collapse = ", "))
    }
  }

  rules <- raw$rules
  if (!is.list(rules) || !length(rules)) {
    add("rules must contain at least one rule")
    return(p)
  }
  ids <- vapply(rules, function(r) as.character(r$id %||% NA), "")
  dup <- unique(ids[duplicated(ids) & !is.na(ids)])
  if (length(dup)) add("duplicate rule id(s): %s", paste(dup, collapse = ", "))

  for (i in seq_along(rules)) {
    r <- rules[[i]]
    where <- if (sg_is_string(r$id)) sprintf("rule %s", r$id) else sprintf("rule #%d", i)
    if (!sg_is_string(r$id)) add("%s: id is required", where)
    for (field in c("check", "dataset", "severity", "why", "guidance", "reference")) {
      if (!sg_is_string(r[[field]])) add("%s: '%s' is required", where, field)
    }
    if (sg_is_string(r$severity) && !r$severity %in% sg_valid_severities) {
      add("%s: severity must be one of %s", where, paste(sg_valid_severities, collapse = ", "))
    }
    if (!is.null(r$blocking) && !(is.logical(r$blocking) && length(r$blocking) == 1L && !is.na(r$blocking))) {
      add("%s: blocking must be true or false", where)
    }
    if (sg_is_string(r$dataset) && !r$dataset %in% names(datasets)) {
      add("%s: dataset '%s' is not declared under datasets", where, r$dataset)
      next
    }
    if (!sg_is_string(r$check)) next
    def <- sg_checks[[r$check]]
    if (is.null(def)) {
      add("%s: unknown check '%s' (available: %s)", where, r$check,
          paste(names(sg_checks), collapse = ", "))
      next
    }
    params <- r$params %||% list()
    missing_params <- setdiff(def$required, names(params))
    if (length(missing_params)) {
      add("%s: check '%s' needs param(s): %s", where, r$check, paste(missing_params, collapse = ", "))
      next
    }
    unknown <- setdiff(names(params), c(def$required, def$optional))
    if (length(unknown)) {
      add("%s: unknown param(s) for check '%s': %s", where, r$check, paste(unknown, collapse = ", "))
    }
    if (!is.null(def$check_params)) {
      msgs <- def$check_params(params, r$dataset, datasets, codelists)
      for (m in msgs) add("%s: %s", where, m)
    }
    if (!is.null(params$where)) {
      w <- params$where
      if (!is.list(w) || !sg_is_string(w$variable) || (is.null(w$equals) && is.null(w[["in"]]))) {
        add("%s: where must be {variable: NAME, equals: VALUE} or {variable: NAME, in: [VALUES]}", where)
      } else if (!w$variable %in% names(datasets[[r$dataset]]$variables)) {
        add("%s: where uses undeclared variable %s.%s", where, r$dataset, w$variable)
      } else if (any(vapply(c(list(w$equals), as.list(w[["in"]])), is.logical, logical(1)))) {
        add("%s: where value parsed as true/false: quote values such as 'Y' in YAML", where)
      }
    }
  }
  p
}

# Shared param checks used by the check registry.
sg_declared <- function(datasets, ds, vars) {
  bad <- setdiff(vars, names(datasets[[ds]]$variables))
  if (length(bad)) sprintf("variable(s) not declared in %s: %s", ds, paste(bad, collapse = ", ")) else character()
}

sg_declared_parent <- function(datasets, parent) {
  if (!sg_is_string(parent) || !parent %in% names(datasets)) {
    sprintf("parent dataset '%s' is not declared under datasets", parent %||% "")
  } else character()
}

#' @export
print.studygate_spec <- function(x, ...) {
  cat(sprintf("StudyGate specification %s v%s\n", x$study$id, x$spec$version))
  if (!is.null(x$study$title)) cat("  ", x$study$title, "\n", sep = "")
  cat(sprintf("  datasets: %s\n", paste(names(x$datasets), collapse = ", ")))
  sev <- table(factor(vapply(x$rules, `[[`, "", "severity"), sg_valid_severities))
  cat(sprintf("  rules: %d (%s)\n", length(x$rules),
              paste(sprintf("%d %s", sev, names(sev)), collapse = ", ")))
  cat(sprintf("  source: %s\n  md5: %s\n", x$source, x$hash))
  invisible(x)
}

#' List the rules in a specification
#'
#' @param specification A path, list or `studygate_spec`.
#' @return A data frame with one row per rule.
#' @examples
#' list_rules(studygate_example("study_b.yml"))
#' @export
list_rules <- function(specification) {
  spec <- read_spec(specification)
  data.frame(
    id = names(spec$rules),
    dataset = vapply(spec$rules, `[[`, "", "dataset"),
    check = vapply(spec$rules, `[[`, "", "check"),
    severity = vapply(spec$rules, `[[`, "", "severity"),
    blocking = vapply(spec$rules, function(r) isTRUE(r$blocking), logical(1)),
    title = vapply(spec$rules, `[[`, "", "title"),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Path to a file shipped with studygate
#'
#' @param file A file name such as `"study_a.yml"`. Specifications live in
#'   `extdata/specs`; omit `file` to list them.
#' @return A file path (or the available file names).
#' @examples
#' studygate_example()
#' studygate_example("study_a.yml")
#' @export
studygate_example <- function(file = NULL) {
  dir <- system.file("extdata", "specs", package = "studygate")
  if (is.null(file)) return(list.files(dir))
  path <- file.path(dir, file)
  if (!file.exists(path)) stop("no example file named ", file, call. = FALSE)
  path
}
