#!/usr/bin/env Rscript
# Applies ONE documented correction to a copy of the synthetic Study A data,
# as a programmer would after a data-management query is answered. The source
# folder is never modified, and the script stops unless exactly one record
# matches the correction.
#   usage: scripts/apply_correction.R <source dir> <output dir>
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2, requireNamespace("haven", quietly = TRUE))
src <- args[1]; out <- args[2]

correction <- list(
  id = "DQ-SYN-0042", dataset = "adae",
  match = list(USUBJID = "SYN-A-011", AESEQ = 3),
  set = list(USUBJID = "SYN-A-001"),
  reason = "Site confirmed subject ID transposition (synthetic query response)"
)

dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (f in list.files(src, pattern = "\\.xpt$")) file.copy(file.path(src, f), file.path(out, f), overwrite = TRUE)
path <- file.path(out, paste0(correction$dataset, ".xpt"))
d <- haven::read_xpt(path)
hit <- Reduce(`&`, Map(function(v, val) d[[v]] == val, names(correction$match), correction$match))
if (sum(hit) != 1) stop(sprintf("correction %s matches %d records, expected exactly 1", correction$id, sum(hit)))
for (v in names(correction$set)) {
  cat(sprintf("Correction %s: %s USUBJID=%s AESEQ=%s  %s \"%s\" -> \"%s\"\n", correction$id, toupper(correction$dataset),
              correction$match$USUBJID, correction$match$AESEQ, v, d[[v]][hit], correction$set[[v]]))
  d[[v]][hit] <- correction$set[[v]]
}
cat(sprintf("  reason: %s\n  records changed: %d   source folder untouched: %s\n", correction$reason, sum(hit), src))
haven::write_xpt(d, path, version = 5, name = toupper(correction$dataset))
cat(sprintf("  wrote %s\n", path))
