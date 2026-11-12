# Builds the SYNTHETIC example data shipped with studygate.
#
# Every record is written by hand (no random generation) so each intentional
# defect is known in advance and documented here. No real patient data: the
# identifiers, dates, regimens and values are invented.
#
# Run from the package root:  Rscript data-raw/make_examples.R
# Writes data/*.rda and inst/extdata/studies/<study>/{adsl,adae}.xpt

d <- function(x) as.Date(x)

# ---------------------------------------------------------------------------
# Study A: synthetic adult HIV treatment study (SYN-HIV-A)
# 10 subjects, 24 weeks of treatment (TRTEDT = TRTSDT + 167 days).
# SYN-A-009 was randomized but never dosed (SAFFL = "N", no treatment dates).
# ---------------------------------------------------------------------------
a_ids <- sprintf("SYN-A-%03d", 1:10)
a_age <- c(34, 45, 52, 29, 61, 38, 47, 23, 56, 41)
a_trtsdt <- d(c("2025-01-06", "2025-01-08", "2025-01-13", "2025-01-15", "2025-01-20",
                "2025-01-22", "2025-02-03", "2025-02-05", NA, "2025-02-10"))
a_trt <- c("Regimen 1", "Regimen 2", "Regimen 1", "Regimen 2", "Regimen 1",
           "Regimen 2", "Regimen 1", "Regimen 2", "", "Regimen 2")
a_adsl <- data.frame(
  STUDYID = "SYN-HIV-A",
  USUBJID = a_ids,
  SUBJID = sprintf("%03d", 1:10),
  SITEID = c("101", "101", "102", "102", "103", "103", "101", "102", "103", "101"),
  AGE = a_age,
  AGEU = "YEARS",
  AGEGR1 = c("18-34", "35-49", ">=50", "18-34", ">=50", "35-49", "35-49", "18-34", ">=50", "35-49"),
  SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F"),
  TRT01P = c("Regimen 1", "Regimen 2", "Regimen 1", "Regimen 2", "Regimen 1",
             "Regimen 2", "Regimen 1", "Regimen 2", "Regimen 1", "Regimen 2"),
  TRT01A = a_trt,
  SAFFL = c(rep("Y", 8), "N", "Y"),
  TRTSDT = a_trtsdt,
  TRTEDT = a_trtsdt + 167,
  CD4BL = c(412, 288, 530, 196, 645, 350, 221, 478, 390, 305),
  stringsAsFactors = FALSE
)

# AE start/end given as study day offsets from TRTSDT (day 0 = first dose).
a_ae <- function(id, seq, term, bodsys, sev, ser, start_day, end_day, temfl) {
  i <- match(id, a_ids)
  data.frame(
    STUDYID = "SYN-HIV-A", USUBJID = id, AESEQ = seq, AETERM = toupper(term), AEDECOD = term,
    AEBODSYS = bodsys, AESEV = sev, AESER = ser,
    ASTDT = a_trtsdt[i] + start_day, AENDT = a_trtsdt[i] + end_day,
    TRTA = a_trt[i], TRTEMFL = temfl, stringsAsFactors = FALSE
  )
}
NS <- "Nervous system disorders"; GI <- "Gastrointestinal disorders"
GEN <- "General disorders and administration site conditions"
SKIN <- "Skin and subcutaneous tissue disorders"; PSY <- "Psychiatric disorders"
a_adae <- rbind(
  a_ae("SYN-A-001", 1, "Headache",  NS,   "MILD",     "N",   3,   5,  "Y"),
  a_ae("SYN-A-001", 2, "Nausea",    GI,   "MODERATE", "N",  10,  12,  "Y"),
  a_ae("SYN-A-001", 3, "Diarrhoea", GI,   "MILD",     "N",  40,  43,  "Y"),
  a_ae("SYN-A-002", 1, "Diarrhoea", GI,   "MILD",     "N",   0,   4,  "Y"),  # same day as first dose: TE
  a_ae("SYN-A-002", 2, "Insomnia",  PSY,  "MILD",     "N",  -5,   2,  ""),   # before first dose: not TE
  a_ae("SYN-A-003", 1, "Fatigue",   GEN,  "MODERATE", "N",  30,  NA,  "Y"),  # ongoing (no end date)
  a_ae("SYN-A-004", 1, "Rash",      SKIN, "SEVERE",   "Y",  14,  28,  "Y"),
  a_ae("SYN-A-005", 1, "Dizziness", NS,   "MILD",     "N", 170, 172,  "Y"),  # last dose day 167, +30 window: TE
  a_ae("SYN-A-006", 1, "Headache",  NS,   "MILD",     "N", 200, 201,  ""),   # day 200 > 167 + 30: not TE
  a_ae("SYN-A-007", 1, "Nausea",    GI,   "MILD",     "N",   7,   9,  "Y"),
  a_ae("SYN-A-008", 1, "Fatigue",   GEN,  "MILD",     "N",  21,  25,  "Y"),
  a_ae("SYN-A-010", 1, "Headache",  NS,   "MILD",     "N",   2,   3,  "Y")
)

# Demo defect (the ONLY defect in study_a): data entry transposed SYN-A-001 to
# SYN-A-011 on the subject's third AE. SYN-A-011 does not exist in ADSL.
study_a <- list(ADSL = a_adsl, ADAE = a_adae)
study_a$ADAE$USUBJID[study_a$ADAE$USUBJID == "SYN-A-001" & study_a$ADAE$AESEQ == 3] <- "SYN-A-011"

# ---------------------------------------------------------------------------
# Study A defects showcase: study A plus one documented defect per rule type.
# Expected findings are listed in tests/testthat/test-examples.R and were
# worked out by hand from this list, not copied from engine output.
# ---------------------------------------------------------------------------
x_adsl <- a_adsl
x_adsl$AGE[x_adsl$USUBJID == "SYN-A-008"] <- 17          # D1 below adult range; also no AGEGR1 bin
x_adsl$SEX[x_adsl$USUBJID == "SYN-A-006"] <- "Female"    # D2 not in SEX codelist
x_adsl$AGEGR1[x_adsl$USUBJID == "SYN-A-007"] <- ">=50"   # D3 AGE 47 belongs in 35-49
x_adsl$TRTSDT[x_adsl$USUBJID == "SYN-A-010"] <- NA       # D4 dosed subject with no first-dose date
x_adsl$CD4BL <- as.character(x_adsl$CD4BL)               # D5 CD4 stored as text, with a censored value
x_adsl$CD4BL[x_adsl$USUBJID == "SYN-A-004"] <- "<50"

x_adae <- study_a$ADAE                                   # E1 orphan SYN-A-011 kept from study_a
x_adae$AESEV[x_adae$USUBJID == "SYN-A-004"] <- "Severe"  # E2 wrong case for controlled term
i8 <- x_adae$USUBJID == "SYN-A-008"
x_adae$AENDT[i8] <- x_adae$ASTDT[i8] - 2                 # E3 end before start
x_adae$TRTEMFL[x_adae$USUBJID == "SYN-A-002" & x_adae$AESEQ == 2] <- "Y"  # E4 pre-dose AE flagged
x_adae$TRTEMFL[x_adae$USUBJID == "SYN-A-007"] <- ""      # E5 on-treatment AE not flagged
x_adae$TRTA[x_adae$USUBJID == "SYN-A-003"] <- "Regimen 2"  # E6 TRTA disagrees with ADSL.TRT01A
study_a_defects <- list(ADSL = x_adsl, ADAE = x_adae)

# ---------------------------------------------------------------------------
# Study B: synthetic pediatric HIV study (SYN-HIV-B). Clean: every rule passes.
# AGE in MONTHS, weight-band dosing, AE grade on a 1-5 toxicity scale,
# 12 weeks of treatment (TRTEDT = TRTSDT + 83), 14-day TEAE window.
# ---------------------------------------------------------------------------
b_ids <- sprintf("SYN-B-%03d", 1:8)
b_age <- c(14, 30, 70, 72, 110, 150, 23, 200)            # 70/72 and 23 sit on group edges
b_trtsdt <- d(c("2025-03-03", "2025-03-05", "2025-03-10", "2025-03-12",
                "2025-03-17", "2025-03-19", "2025-03-24", "2025-03-26"))
b_trt <- c("Cohort 1", "Cohort 1", "Cohort 2", "Cohort 2", "Cohort 1", "Cohort 2", "Cohort 1", "Cohort 2")
b_adsl <- data.frame(
  STUDYID = "SYN-HIV-B",
  USUBJID = b_ids,
  SUBJID = sprintf("%03d", 1:8),
  SITEID = c("201", "201", "202", "202", "203", "203", "201", "202"),
  AGE = b_age,
  AGEU = "MONTHS",
  AGEGR1 = c("<2 years", "2-<6 years", "2-<6 years", "6-<12 years", "6-<12 years",
             "12-<18 years", "<2 years", "12-<18 years"),
  SEX = c("F", "M", "M", "F", "M", "F", "M", "F"),
  WEIGHTBL = c(9.8, 13.5, 19.0, 21.4, 30.2, 44.0, 11.9, 52.5),
  TRT01P = b_trt,
  TRT01A = b_trt,
  SAFFL = "Y",
  TRTSDT = b_trtsdt,
  TRTEDT = b_trtsdt + 83,
  stringsAsFactors = FALSE
)
b_ae <- function(id, seq, term, bodsys, grade, ser, start_day, end_day, temfl) {
  i <- match(id, b_ids)
  data.frame(
    STUDYID = "SYN-HIV-B", USUBJID = id, AESEQ = seq, AETERM = toupper(term), AEDECOD = term,
    AEBODSYS = bodsys, AETOXGR = grade, AESER = ser,
    ASTDT = b_trtsdt[i] + start_day, AENDT = b_trtsdt[i] + end_day,
    TRTA = b_trt[i], TRTEMFL = temfl, stringsAsFactors = FALSE
  )
}
RESP <- "Respiratory, thoracic and mediastinal disorders"; BLOOD <- "Blood and lymphatic system disorders"
b_adae <- rbind(
  b_ae("SYN-B-001", 1, "Pyrexia",   GEN,   "1", "N",   2,   4, "Y"),
  b_ae("SYN-B-002", 1, "Vomiting",  GI,    "2", "N",   5,   6, "Y"),
  b_ae("SYN-B-003", 1, "Rash",      SKIN,  "1", "N",   0,   3, "Y"),  # first-dose day: TE
  b_ae("SYN-B-004", 1, "Cough",     RESP,  "1", "N",  NA,  NA, "Y"),  # no start date: flagged per convention
  b_ae("SYN-B-005", 1, "Anaemia",   BLOOD, "3", "Y",  28,  40, "Y"),
  b_ae("SYN-B-006", 1, "Headache",  NS,    "1", "N",  95,  96, "Y"),  # last dose day 83, +14 window: TE
  b_ae("SYN-B-007", 1, "Diarrhoea", GI,    "2", "N",  -2,   1, ""),   # before first dose
  b_ae("SYN-B-008", 1, "Pyrexia",   GEN,   "1", "N", 100, 101, "")    # day 100 > 83 + 14
)
study_b <- list(ADSL = b_adsl, ADAE = b_adae)

save(study_a, file = "data/study_a.rda", compress = "xz", version = 3)
save(study_b, file = "data/study_b.rda", compress = "xz", version = 3)
save(study_a_defects, file = "data/study_a_defects.rda", compress = "xz", version = 3)

# SAS transport copies for the command-line demo (the CLI reads a folder).
for (s in c("study_a", "study_b")) {
  out <- file.path("inst", "extdata", "studies", s)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  obj <- get(s)
  for (ds in names(obj)) haven::write_xpt(obj[[ds]], file.path(out, paste0(tolower(ds), ".xpt")),
                                          version = 5, name = ds)
}
message("wrote data/*.rda and inst/extdata/studies/*/*.xpt")
