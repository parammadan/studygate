# The check registry.
#
# Each check type is study-agnostic: it receives the datasets and one rule from
# a specification, and returns the records that break the rule. Studies differ
# only in their specifications, so a new study never needs new check code.
#
# A check definition has:
#   description   one line shown by list_checks()
#   required      params the rule must supply
#   optional      params the rule may supply
#   check_params  function(params, dataset, datasets, codelists) -> problems,
#                 run by read_spec() before any data are touched
#   run           function(ctx) -> data frame of findings (see sg_findings());
#                 calls sg_not_evaluable() when the data cannot support the rule
#   r_code        function(rule, spec) -> R code a programmer can paste to see the rows
#   sas_code      function(rule, spec) -> an illustrative SAS analogue (not executed)
#
# ctx fields: data (the rule's dataset), datasets (all), rule, params, spec,
# dataset (name), key (key variables of the dataset).

sg_checks <- list()

sg_lc <- function(x) tolower(x)

# Quote values for an R vector literal: c("A", "B").
sg_qv <- function(x) paste0('"', gsub('"', '\\"', as.character(x), fixed = TRUE), '"', collapse = ", ")

# First line of a generated R snippet: x <- the dataset, filtered by the rule's
# where clause if it has one, so the snippet shows exactly the rows checked.
sg_r_subset <- function(rule) {
  d <- sg_lc(rule$dataset)
  w <- rule$params$where
  if (is.null(w)) return(sprintf("x <- %s\n", d))
  vals <- if (!is.null(w$equals)) w$equals else unlist(w[["in"]])
  sprintf("x <- %s[%s$%s %%in%% c(%s), ]\n", d, d, w$variable, sg_qv(vals))
}

# ---- required_variables -----------------------------------------------------
sg_checks$required_variables <- list(
  description = "Variables declared as required are present in the dataset.",
  required = character(),
  optional = "variables",
  check_params = function(params, ds, datasets, codelists) {
    sg_declared(datasets, ds, unlist(params$variables))
  },
  run = function(ctx) {
    vars <- sg_required_vars(ctx)
    absent <- setdiff(vars, names(ctx$data))
    sg_findings(
      row = rep(NA_integer_, length(absent)), key = NA, variable = absent,
      expected = "present", observed = "absent",
      message = sprintf("%s.%s is required but not present", ctx$dataset, absent)
    )
  },
  r_code = function(rule, spec) {
    vars <- sg_required_vars(list(params = rule$params, spec = spec, dataset = rule$dataset))
    sprintf("setdiff(c(%s), names(%s))", sg_qv(vars), sg_lc(rule$dataset))
  },
  sas_code = function(rule, spec) {
    sprintf("proc contents data=%s out=_vars(keep=name) noprint; run;  /* compare _vars to the spec */",
            rule$dataset)
  }
)

sg_required_vars <- function(ctx) {
  vars <- unlist(ctx$params$variables)
  if (is.null(vars)) {
    decl <- ctx$spec$datasets[[ctx$dataset]]$variables
    vars <- names(decl)[vapply(decl, function(v) isTRUE(v$required), logical(1))]
  }
  vars
}

# ---- variable_types ---------------------------------------------------------
sg_checks$variable_types <- list(
  description = "Present variables have the type declared in the specification (character, numeric, date).",
  required = character(),
  optional = character(),
  run = function(ctx) {
    decl <- ctx$spec$datasets[[ctx$dataset]]$variables
    vars <- intersect(names(decl), names(ctx$data))
    declared <- vapply(vars, function(v) decl[[v]]$type, "")
    actual <- vapply(vars, function(v) sg_type_of(ctx$data[[v]]), "")
    bad <- declared != actual
    sg_findings(
      row = rep(NA_integer_, sum(bad)), key = NA, variable = vars[bad],
      expected = declared[bad], observed = actual[bad],
      message = sprintf("%s.%s is %s; the specification declares %s",
                        ctx$dataset, vars[bad], actual[bad], declared[bad])
    )
  },
  r_code = function(rule, spec) sprintf("vapply(%s, function(x) class(x)[1], character(1))", sg_lc(rule$dataset)),
  sas_code = function(rule, spec) sprintf("proc contents data=%s; run;", rule$dataset)
)

# ---- not_missing ------------------------------------------------------------
sg_checks$not_missing <- list(
  description = "Listed variables have no missing values (NA or blank), optionally within a subset.",
  required = "variables",
  optional = "where",
  check_params = function(params, ds, datasets, codelists) {
    sg_declared(datasets, ds, unlist(params$variables))
  },
  run = function(ctx) {
    vars <- unlist(ctx$params$variables)
    sg_require_columns(ctx$data, vars, ctx$dataset)
    rows <- sg_where_rows(ctx$data, ctx$params$where, ctx$dataset)
    out <- lapply(vars, function(v) {
      r <- rows[sg_is_missing(ctx$data[[v]][rows])]
      sg_findings(r, sg_record_key(ctx$data, r, ctx$key), v, "non-missing", "<missing>",
                  sprintf("%s is missing", v))
    })
    do.call(rbind, out)
  },
  r_code = function(rule, spec) {
    paste0(sg_r_subset(rule), sprintf(
      'x[Reduce(`|`, lapply(x[c(%s)], function(v) is.na(v) | trimws(v) == "")), ]',
      sg_qv(unlist(rule$params$variables))))
  },
  sas_code = function(rule, spec) {
    v <- unlist(rule$params$variables)[1]
    sprintf("data _miss; set %s; if missing(%s); run;", rule$dataset, v)
  }
)

# ---- unique_key -------------------------------------------------------------
sg_checks$unique_key <- list(
  description = "Key variables identify each record uniquely and are never missing.",
  required = character(),
  optional = "by",
  check_params = function(params, ds, datasets, codelists) {
    by <- unlist(params$by %||% datasets[[ds]]$key)
    c(if (!length(by)) "no key: give params.by or declare datasets.<name>.key",
      sg_declared(datasets, ds, by))
  },
  run = function(ctx) {
    by <- unlist(ctx$params$by %||% ctx$key)
    sg_require_columns(ctx$data, by, ctx$dataset)
    keys <- do.call(paste, c(lapply(by, function(v) sg_fmt(ctx$data[[v]])), sep = "\r"))
    any_missing <- Reduce(`|`, lapply(by, function(v) sg_is_missing(ctx$data[[v]])))
    dup_rows <- which(keys %in% keys[duplicated(keys)] & !any_missing)
    counts <- table(keys)
    miss_rows <- which(any_missing)
    rbind(
      sg_findings(dup_rows, sg_record_key(ctx$data, dup_rows, by), paste(by, collapse = ", "),
                  "1 record per key", sprintf("%d records", as.integer(counts[keys[dup_rows]])),
                  "duplicate key"),
      sg_findings(miss_rows, sg_record_key(ctx$data, miss_rows, by), paste(by, collapse = ", "),
                  "complete key", "key value missing", "key value missing")
    )
  },
  r_code = function(rule, spec) {
    by <- unlist(rule$params$by %||% spec$datasets[[rule$dataset]]$key)
    d <- sg_lc(rule$dataset)
    sprintf("%s[duplicated(%s[c(%s)]) | duplicated(%s[c(%s)], fromLast = TRUE), ]",
            d, d, paste0('"', by, '"', collapse = ", "), d, paste0('"', by, '"', collapse = ", "))
  },
  sas_code = function(rule, spec) {
    by <- paste(unlist(rule$params$by %||% spec$datasets[[rule$dataset]]$key), collapse = " ")
    sprintf("proc sort data=%s out=_s nouniquekey; by %s; run;", rule$dataset, by)
  }
)

# ---- codelist ---------------------------------------------------------------
sg_checks$codelist <- list(
  description = "Non-missing values come from a controlled-terminology codelist in the specification (exact, case-sensitive).",
  required = c("variable", "codelist"),
  optional = "where",
  check_params = function(params, ds, datasets, codelists) {
    c(sg_declared(datasets, ds, params$variable),
      if (!params$codelist %in% names(codelists)) sprintf("codelist '%s' is not defined", params$codelist))
  },
  run = function(ctx) {
    v <- ctx$params$variable
    sg_require_columns(ctx$data, v, ctx$dataset)
    allowed <- ctx$spec$codelists[[ctx$params$codelist]]
    rows <- sg_where_rows(ctx$data, ctx$params$where, ctx$dataset)
    x <- ctx$data[[v]]
    bad <- rows[!sg_is_missing(x[rows]) & !(sg_fmt(x[rows]) %in% allowed)]
    hint <- vapply(sg_fmt(x[bad]), function(val) {
      near <- allowed[toupper(allowed) == toupper(trimws(val))]
      if (length(near)) sprintf(" (did you mean \"%s\"?)", near[1]) else ""
    }, "")
    sg_findings(bad, sg_record_key(ctx$data, bad, ctx$key), v,
                paste0("one of ", ctx$params$codelist, ": ", paste(allowed, collapse = ", ")),
                sg_fmt(x[bad]),
                sprintf("\"%s\" is not in codelist %s%s", sg_fmt(x[bad]), ctx$params$codelist, hint))
  },
  r_code = function(rule, spec) {
    v <- rule$params$variable
    paste0(sg_r_subset(rule), sprintf('x[!is.na(x$%s) & trimws(x$%s) != "" & !x$%s %%in%% c(%s), ]',
                                      v, v, v, sg_qv(spec$codelists[[rule$params$codelist]])))
  },
  sas_code = function(rule, spec) {
    sprintf("proc freq data=%s; tables %s / missing; run;  /* compare to codelist %s */",
            rule$dataset, rule$params$variable, rule$params$codelist)
  }
)

# ---- range ------------------------------------------------------------------
sg_checks$range <- list(
  description = "Non-missing numeric values lie within [min, max] (inclusive).",
  required = "variable",
  optional = c("min", "max", "where"),
  check_params = function(params, ds, datasets, codelists) {
    c(sg_declared(datasets, ds, params$variable),
      if (is.null(params$min) && is.null(params$max)) "range needs min and/or max",
      if (!is.null(params$min) && !is.numeric(params$min)) "min must be a number",
      if (!is.null(params$max) && !is.numeric(params$max)) "max must be a number")
  },
  run = function(ctx) {
    v <- ctx$params$variable
    sg_require_columns(ctx$data, v, ctx$dataset)
    sg_require_type(ctx$data, v, "numeric", ctx$dataset)
    lo <- ctx$params$min %||% -Inf
    hi <- ctx$params$max %||% Inf
    rows <- sg_where_rows(ctx$data, ctx$params$where, ctx$dataset)
    x <- ctx$data[[v]]
    bad <- rows[!is.na(x[rows]) & (x[rows] < lo | x[rows] > hi)]
    sg_findings(bad, sg_record_key(ctx$data, bad, ctx$key), v,
                sprintf("between %s and %s", format(lo), format(hi)), sg_fmt(x[bad]),
                sprintf("%s = %s is outside [%s, %s]", v, sg_fmt(x[bad]), format(lo), format(hi)))
  },
  r_code = function(rule, spec) {
    v <- rule$params$variable
    paste0(sg_r_subset(rule), sprintf("x[!is.na(x$%s) & (x$%s < %s | x$%s > %s), ]", v, v,
                                      rule$params$min %||% -Inf, v, rule$params$max %||% Inf))
  },
  sas_code = function(rule, spec) {
    v <- rule$params$variable
    sprintf("data _out; set %s; if not missing(%s) and not (%s <= %s <= %s); run;",
            rule$dataset, v, rule$params$min %||% ".", v, rule$params$max %||% ".")
  }
)

# ---- subject_in_parent ------------------------------------------------------
sg_checks$subject_in_parent <- list(
  description = "Every record's subject identifier exists in the parent (subject-level) dataset.",
  required = "parent",
  optional = "by",
  check_params = function(params, ds, datasets, codelists) {
    by <- params$by %||% "USUBJID"
    msgs <- sg_declared_parent(datasets, params$parent)
    if (length(msgs)) return(msgs)
    c(sg_declared(datasets, ds, by), sg_declared(datasets, params$parent, by))
  },
  run = function(ctx) {
    by <- ctx$params$by %||% "USUBJID"
    parent <- sg_get_parent(ctx)
    sg_require_columns(ctx$data, by, ctx$dataset)
    sg_require_columns(parent, by, ctx$params$parent)
    x <- ctx$data[[by]]
    known <- sg_fmt(parent[[by]][!sg_is_missing(parent[[by]])])
    miss <- which(sg_is_missing(x))
    orphan <- which(!sg_is_missing(x) & !(sg_fmt(x) %in% known))
    rbind(
      sg_findings(orphan, sg_record_key(ctx$data, orphan, ctx$key), by,
                  sprintf("a %s present in %s", by, ctx$params$parent), sg_fmt(x[orphan]),
                  sprintf("%s %s has %s records but no %s record", by, sg_fmt(x[orphan]),
                          ctx$dataset, ctx$params$parent)),
      sg_findings(miss, sg_record_key(ctx$data, miss, ctx$key), by,
                  "non-missing", "<missing>", sprintf("%s is missing", by))
    )
  },
  r_code = function(rule, spec) {
    by <- rule$params$by %||% "USUBJID"; d <- sg_lc(rule$dataset); p <- sg_lc(rule$params$parent)
    sprintf("%s[!%s$%s %%in%% %s$%s, ]", d, d, by, p, by)
  },
  sas_code = function(rule, spec) {
    by <- rule$params$by %||% "USUBJID"
    sprintf("proc sql; select * from %s where %s not in (select %s from %s); quit;",
            rule$dataset, by, by, rule$params$parent)
  }
)

sg_get_parent <- function(ctx) {
  p <- ctx$datasets[[ctx$params$parent]]
  if (is.null(p)) sg_not_evaluable(sprintf("parent dataset %s was not supplied", ctx$params$parent))
  p
}

# Match child rows to a parent that must be unique by `by`. Returns the parent
# row index for each child row (NA when there is no parent record).
sg_match_parent <- function(ctx, parent, by) {
  pk <- sg_fmt(parent[[by]])
  pk[sg_is_missing(parent[[by]])] <- NA
  if (anyDuplicated(pk[!is.na(pk)])) {
    sg_not_evaluable(sprintf(
      "%s is not unique by %s, so %s records cannot be matched to one parent record; resolve the duplicate-key finding first",
      ctx$params$parent, by, ctx$dataset))
  }
  ck <- sg_fmt(ctx$data[[by]])
  ck[sg_is_missing(ctx$data[[by]])] <- NA
  match(ck, pk, incomparables = NA)
}

# ---- parent_consistency -----------------------------------------------------
sg_checks$parent_consistency <- list(
  description = "Variables carried from the parent dataset equal the parent's values (for example ADAE.TRTA = ADSL.TRT01A).",
  required = c("parent", "variables"),
  optional = "by",
  check_params = function(params, ds, datasets, codelists) {
    msgs <- sg_declared_parent(datasets, params$parent)
    if (length(msgs)) return(msgs)
    by <- params$by %||% "USUBJID"
    if (!is.list(params$variables) || is.null(names(params$variables))) {
      return("variables must map child variables to parent variables, e.g. {TRTA: TRT01A}")
    }
    c(sg_declared(datasets, ds, c(by, names(params$variables))),
      sg_declared(datasets, params$parent, c(by, unlist(params$variables))))
  },
  run = function(ctx) {
    by <- ctx$params$by %||% "USUBJID"
    map <- ctx$params$variables
    parent <- sg_get_parent(ctx)
    sg_require_columns(ctx$data, c(by, names(map)), ctx$dataset)
    sg_require_columns(parent, c(by, unlist(map)), ctx$params$parent)
    idx <- sg_match_parent(ctx, parent, by)
    out <- lapply(names(map), function(cv) {
      pv <- map[[cv]]
      have <- which(!is.na(idx))   # rows without a parent belong to subject_in_parent
      child <- sg_fmt(ctx$data[[cv]][have])
      par <- sg_fmt(parent[[pv]][idx[have]])
      bad <- have[child != par]
      sg_findings(bad, sg_record_key(ctx$data, bad, ctx$key), cv,
                  sg_fmt(parent[[pv]][idx[bad]]), sg_fmt(ctx$data[[cv]][bad]),
                  sprintf("%s.%s does not match %s.%s for this subject", ctx$dataset, cv, ctx$params$parent, pv))
    })
    do.call(rbind, out)
  },
  r_code = function(rule, spec) {
    by <- rule$params$by %||% "USUBJID"; d <- sg_lc(rule$dataset); p <- sg_lc(rule$params$parent)
    cv <- names(rule$params$variables)[1]; pv <- rule$params$variables[[1]]
    sprintf('m <- merge(%s, %s[c("%s", "%s")], by = "%s"); m[m$%s != m$%s, ]', d, p, by, pv, by, cv, pv)
  },
  sas_code = function(rule, spec) {
    by <- rule$params$by %||% "USUBJID"
    cv <- names(rule$params$variables)[1]; pv <- rule$params$variables[[1]]
    sprintf("proc sql; select a.* from %s a join %s b on a.%s=b.%s where a.%s ne b.%s; quit;",
            rule$dataset, rule$params$parent, by, by, cv, pv)
  }
)

# ---- date_order -------------------------------------------------------------
sg_checks$date_order <- list(
  description = "An end date is on or after its start date when both are present.",
  required = c("start", "end"),
  optional = "where",
  check_params = function(params, ds, datasets, codelists) {
    sg_declared(datasets, ds, c(params$start, params$end))
  },
  run = function(ctx) {
    s <- ctx$params$start; e <- ctx$params$end
    sg_require_columns(ctx$data, c(s, e), ctx$dataset)
    sg_require_type(ctx$data, s, "date", ctx$dataset)
    sg_require_type(ctx$data, e, "date", ctx$dataset)
    rows <- sg_where_rows(ctx$data, ctx$params$where, ctx$dataset)
    sd <- ctx$data[[s]][rows]; ed <- ctx$data[[e]][rows]
    bad <- rows[!is.na(sd) & !is.na(ed) & ed < sd]
    sg_findings(bad, sg_record_key(ctx$data, bad, ctx$key), e,
                sprintf("%s >= %s (%s)", e, s, sg_fmt(ctx$data[[s]][bad])),
                sg_fmt(ctx$data[[e]][bad]),
                sprintf("%s is before %s", e, s))
  },
  r_code = function(rule, spec) {
    s <- rule$params$start; e <- rule$params$end
    paste0(sg_r_subset(rule), sprintf("x[!is.na(x$%s) & !is.na(x$%s) & x$%s < x$%s, ]", s, e, e, s))
  },
  sas_code = function(rule, spec) {
    sprintf("data _out; set %s; if n(%s, %s) = 2 and %s < %s; run;", rule$dataset,
            rule$params$start, rule$params$end, rule$params$end, rule$params$start)
  }
)

# ---- treatment_emergent -----------------------------------------------------
sg_checks$treatment_emergent <- list(
  description = paste(
    "A treatment-emergent flag matches the study's approved rule: flagged when the",
    "event starts on or after first dose and no later than last dose + window_days."
  ),
  required = c("flag", "start", "parent", "trt_start", "trt_end", "window_days", "missing_start"),
  optional = c("by", "flag_value"),
  check_params = function(params, ds, datasets, codelists) {
    msgs <- sg_declared_parent(datasets, params$parent)
    if (length(msgs)) return(msgs)
    by <- params$by %||% "USUBJID"
    c(sg_declared(datasets, ds, c(by, params$flag, params$start)),
      sg_declared(datasets, params$parent, c(by, params$trt_start, params$trt_end)),
      if (!is.numeric(params$window_days) || length(params$window_days) != 1 || params$window_days < 0)
        "window_days must be a number >= 0",
      if (!identical(params$missing_start, "flag") && !identical(params$missing_start, "do_not_flag"))
        "missing_start must be 'flag' or 'do_not_flag' (the approved convention for events with no start date)")
  },
  run = function(ctx) {
    p <- ctx$params
    by <- p$by %||% "USUBJID"
    yes <- p$flag_value %||% "Y"
    parent <- sg_get_parent(ctx)
    sg_require_columns(ctx$data, c(by, p$flag, p$start), ctx$dataset)
    sg_require_columns(parent, c(by, p$trt_start, p$trt_end), p$parent)
    sg_require_type(ctx$data, p$start, "date", ctx$dataset)
    sg_require_type(parent, p$trt_start, "date", p$parent)
    sg_require_type(parent, p$trt_end, "date", p$parent)
    idx <- sg_match_parent(ctx, parent, by)
    have <- which(!is.na(idx))
    astdt <- ctx$data[[p$start]][have]
    trtsdt <- parent[[p$trt_start]][idx[have]]
    trtedt <- parent[[p$trt_end]][idx[have]]

    expected <- ifelse(astdt >= trtsdt & astdt <= trtedt + p$window_days, yes, "")
    expected[is.na(astdt)] <- if (p$missing_start == "flag") yes else ""
    cannot <- !is.na(astdt) & (is.na(trtsdt) | is.na(trtedt))
    expected[cannot] <- NA

    flag <- ctx$data[[p$flag]][have]
    observed <- ifelse(sg_is_missing(flag), "", as.character(flag))
    bad_flag <- !cannot & observed != expected
    show <- function(v) ifelse(is.na(v) | v == "", "<missing>", v)
    rule_text <- sprintf("%s = \"%s\" when %s >= %s and %s <= %s + %s days",
                         p$flag, yes, p$start, p$trt_start, p$start, p$trt_end, format(p$window_days))
    r1 <- have[bad_flag]
    r2 <- have[cannot]
    rbind(
      sg_findings(r1, sg_record_key(ctx$data, r1, ctx$key), p$flag,
                  show(expected[bad_flag]), show(observed[bad_flag]),
                  sprintf("%s disagrees with the approved rule: %s (%s = %s; %s %s to %s)",
                          p$flag, rule_text, p$start, sg_fmt(astdt[bad_flag]), "treatment",
                          sg_fmt(trtsdt[bad_flag]), sg_fmt(trtedt[bad_flag]))),
      sg_findings(r2, sg_record_key(ctx$data, r2, ctx$key), p$flag,
                  "derivable flag", "cannot be derived",
                  sprintf("%s cannot be confirmed: %s or %s is missing in %s for this subject",
                          p$flag, p$trt_start, p$trt_end, p$parent))
    )
  },
  r_code = function(rule, spec) {
    p <- rule$params; by <- p$by %||% "USUBJID"; d <- sg_lc(rule$dataset); pa <- sg_lc(p$parent)
    paste0(
      sprintf('m <- merge(%s, %s[c("%s", "%s", "%s")], by = "%s")\n', d, pa, by, p$trt_start, p$trt_end, by),
      sprintf('m$exp <- ifelse(m$%s >= m$%s & m$%s <= m$%s + %s, "Y", "")\n',
              p$start, p$trt_start, p$start, p$trt_end, format(p$window_days)),
      sprintf('m[m$exp != ifelse(is.na(m$%s), "", m$%s), ]', p$flag, p$flag)
    )
  },
  sas_code = function(rule, spec) {
    p <- rule$params; by <- p$by %||% "USUBJID"
    sprintf(paste(
      "proc sql; create table _te as select a.*, b.%s, b.%s from %s a left join %s b on a.%s=b.%s; quit;",
      "data _out; set _te; length _exp $1; if %s <= %s <= %s + %s then _exp='Y'; if _exp ne %s; run;"),
      p$trt_start, p$trt_end, rule$dataset, p$parent, by, by,
      p$trt_start, p$start, p$trt_end, format(p$window_days), p$flag)
  }
)

# ---- categorization ---------------------------------------------------------
sg_checks$categorization <- list(
  description = "A derived category (for example AGEGR1) agrees with its numeric source and the specification's cut points.",
  required = c("variable", "source", "categories"),
  optional = character(),
  check_params = function(params, ds, datasets, codelists) {
    cats <- params$categories
    ok <- is.list(cats) && length(cats) &&
      all(vapply(cats, function(c) sg_is_string(c$label) && (!is.null(c$lower) || !is.null(c$upper)), logical(1)))
    c(sg_declared(datasets, ds, c(params$variable, params$source)),
      if (!ok) "categories must be a list of {label, lower (inclusive), upper (exclusive)}")
  },
  run = function(ctx) {
    p <- ctx$params
    sg_require_columns(ctx$data, c(p$variable, p$source), ctx$dataset)
    sg_require_type(ctx$data, p$source, "numeric", ctx$dataset)
    src <- ctx$data[[p$source]]
    expected <- rep(NA_character_, length(src))
    hits <- integer(length(src))
    for (cat in p$categories) {
      lo <- cat$lower %||% -Inf; hi <- cat$upper %||% Inf
      inside <- !is.na(src) & src >= lo & src < hi
      expected[inside] <- cat$label
      hits <- hits + inside
    }
    if (any(hits > 1)) sg_not_evaluable("categories overlap; fix the cut points in the specification")
    observed <- sg_fmt(ctx$data[[p$variable]])
    exp_show <- ifelse(is.na(src), "<missing>", ifelse(is.na(expected), "(no category covers this value)", expected))
    bad <- which(observed != exp_show)
    sg_findings(bad, sg_record_key(ctx$data, bad, ctx$key), p$variable,
                exp_show[bad], observed[bad],
                sprintf("%s = %s should be categorised as %s", p$source, sg_fmt(src[bad]), exp_show[bad]))
  },
  r_code = function(rule, spec) {
    p <- rule$params; d <- sg_lc(rule$dataset)
    sprintf("table(%s$%s, %s$%s, useNA = \"ifany\")", d, p$source, d, p$variable)
  },
  sas_code = function(rule, spec) {
    sprintf("proc freq data=%s; tables %s*%s / list missing; run;", rule$dataset,
            rule$params$source, rule$params$variable)
  }
)

#' List the available check types
#'
#' Check types are the reusable building blocks of the engine. A
#' specification combines them into study-specific rules.
#'
#' @return A data frame with the check name, its parameters and a description.
#' @examples
#' list_checks()
#' @export
list_checks <- function() {
  data.frame(
    check = names(sg_checks),
    required_params = vapply(sg_checks, function(d) paste(d$required, collapse = ", "), ""),
    optional_params = vapply(sg_checks, function(d) paste(d$optional, collapse = ", "), ""),
    description = vapply(sg_checks, `[[`, "", "description"),
    row.names = NULL, stringsAsFactors = FALSE
  )
}
