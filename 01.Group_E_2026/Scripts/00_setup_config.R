# ==============================================================================
# 00_setup_config.R
# TAFITI99 | GROUP E | Shared set-up, constants, metadata loader and helpers
# ------------------------------------------------------------------------------
# Sourced by every pipeline script (01, 04, 05, 06, 07) and by the Quarto layer.
# It contains NO analysis. It holds:
#   1. packages and project paths (all relative to the project root via here())
#   2. analytical switches (knowledge-key HOLD switch, alpha, CI method)
#   3. the dataset-name -> Master Metadata variable_id map
#   4. the predictor specification (labels, populations, REFERENCE categories)
#   5. small formatting helpers shared by scripts and by the report
#
# Governing documents
#   resources/TAFITI99_Master_Research_Metadata_v3.xlsx
#   resources/GroupE_Chapter4_Blueprint_v5.xlsx
# Book: SPSS & R for Health Research - Ch 11, 15 (modified Poisson), 19, 20.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(readr)
  library(readxl)
  library(ggplot2)
  library(here)
})

options(stringsAsFactors = FALSE, scipen = 999, width = 120)

# ------------------------------------------------------------------------------
# 1. PATHS  (relative to the Group E project root; RDS files in results/rds/)
# ------------------------------------------------------------------------------
P <- list(
  raw_xlsx  = here("data", "class_work_2_raw.xlsx"),
  sps       = here("data", "class work 2 - SPSS labels.sps"),
  legacy_csv = here("data", "groupE_cleaned.csv"),      # produced by the earlier Group E.R
  metadata  = here("resources", "TAFITI99_Master_Research_Metadata_v3.xlsx"),
  rds       = here("results", "rds"),
  tables    = here("results", "tables"),
  fig       = here("figures"),
  fig_diag  = here("figures", "diagnostics"),
  reports   = here("reports")
)
invisible(lapply(c(P$rds, P$tables, P$fig, P$fig_diag, P$reports),
                 dir.create, showWarnings = FALSE, recursive = TRUE))

rds_path <- function(name) file.path(P$rds, paste0(name, ".rds"))

# RDS checkpoint names (Blueprint v5, sheet RDS_Checkpoints)
RDS <- list(
  raw        = "data_01_imported_raw",
  audit      = "audit_02_exploration",
  cleaned    = "data_03_cleaned_checked",
  ready      = "data_04_analysis_ready",
  descr      = "results_04_descriptive",
  bivar      = "results_05_bivariate",
  mp_unadj   = "models_06a_modified_poisson_unadjusted",
  mp_adj     = "models_06b_modified_poisson_adjusted",
  mp_all     = "results_06c_modified_poisson_cPR_aPR",
  tbl_obj    = "results_07_table_objects",
  fig_data   = "results_07_figure_data"
)

# ------------------------------------------------------------------------------
# 2. ANALYTICAL SWITCHES
# ------------------------------------------------------------------------------
# KNOWLEDGE-KEY SWITCH  (Verified against Scripts/Group E.R lines 715-756;
# Master Metadata derivations V041/V042; Blueprint design decision 7).
#   TRUE (active): V041 knowledge_score counts "Yes" (code 1 = aware / knows this
#         advantage / knows this side effect, per Group E.R & Master Metadata) as
#         1 point across the 18 prespecified items (V010-V023, V025-V028; 0 missing
#         items in N = 419), and V042 knowledge_level classifies score >= mean(score)
#         as "High Knowledge (>= mean)" vs "Low Knowledge (< mean)".
#   FALSE: places V041/V042 rows on hold in Tables 4.3, 4.4 and 4.5.
KNOWLEDGE_KEY_VERIFIED <- TRUE

ALPHA            <- 0.05   # two-sided significance level
CANDIDATE_P      <- 0.25   # bivariate P threshold for multivariable candidacy
SKEW_RULE        <- 0.5    # |skewness| < 0.5 -> mean (SD); otherwise median (Q1-Q3)
PREV_CI_METHOD   <- "Wilson score interval"   # documented CI method for prevalence
ROBUST_VCOV_TYPE <- "HC0"  # sandwich estimator used for modified Poisson (Book Ch 15)
SEED             <- 20260101

# ------------------------------------------------------------------------------
# 3. DATASET NAME  ->  MASTER METADATA variable_id
#    (dataset names are truncated Kobo/SPSS names; metadata keeps clean names)
# ------------------------------------------------------------------------------
SOURCE_MAP <- tribble(
  ~dataset_name,    ~variable_id,
  "ID_NUMBE",       "V001", "QNA1_AGE",  "V002", "QNA2_EDU",  "V003", "QNA3_REL",  "V004",
  "QNA4_MAR",       "V005", "QNA5_POL",  "V006", "QNA6_OCC",  "V007", "QNA7_NO",   "V008",
  "QNA8_CHI",       "V009",
  "QNB1A_CO",       "V010", "QNB1B_CO",  "V011", "QNB1C_CO",  "V012", "QNB1D_CO",  "V013",
  "QNB1E_CO",       "V014", "QNB1F_CO",  "V015",
  "QNB2A_ADY",      "V016", "QNB2B_ADK", "V017", "QNB2C_ADR", "V018", "QNB2D_ADM", "V019",
  "QNB2E_ADI",      "V020", "QNB2F_ADP", "V021", "QNB2G_ADP", "V022", "QNB2H_ADe", "V023",
  "QNB2I_SPECIFY",  "V024",
  "QNB3A_SIh",      "V025", "QNB3B_SIS", "V026", "QNB3C_SIQ", "V027", "QNB3D_SIL", "V028",
  "QNB3E_OTH",      "V029", "QNB3E_SPECIFY", "V030",
  "QNC1_CURT",      "V031", "QNC2_CON",  "V032",
  "QND1_HUSH",      "V033", "QND2_DEC4", "V034", "QND3_COM",  "V035",
  "QND4_MYTA",      "V036", "QND5_MYT2", "V037", "QND6_MYTG", "V038", "QND7_MANT", "V039",
  "QND8_REL3",      "V040"
)

# Dataset columns that are administrative / not part of Master Metadata
# (QNB2I_OTH, a Kobo "any other advantage mentioned" flag, has no metadata variable_id;
#  it is kept through cleaning and dropped at the transform stage)
ADMIN_COLS <- c("enumerator_id", "note_knowledge", "note_adv", "note_side",
                "_id", "_uuid", "_submission_time", "_validation_status", "_notes",
                "_status", "_submitted_by", "__version__", "_tags", "meta/rootUuid", "_index")

# The 18 knowledge items (Metadata derivation V041)
KNOWLEDGE_IDS <- c(sprintf("V%03d", 10:23), sprintf("V%03d", 25:28))

# ------------------------------------------------------------------------------
# 4. MASTER METADATA LOADER
# ------------------------------------------------------------------------------
read_metadata <- function(path = P$metadata) {
  sheets <- c("study", "variables", "categories", "rules", "derivations", "analysis_plan")
  md <- lapply(sheets, function(s) read_excel(path, sheet = s, col_types = "text"))
  names(md) <- sheets
  md$categories <- md$categories %>% mutate(code = as.integer(code))
  md
}

# ------------------------------------------------------------------------------
# 5. PREDICTOR SPECIFICATION  (Tables 4.4 and 4.5)
#    ref_label = REFERENCE category for the modified Poisson models.
#    Master Metadata does not specify reference categories (Blueprint decision 7),
#    so they are fixed here, once, by an explicit rule and recorded in the
#    results RDS / report notes for confirmation:
#       ordinal predictor           -> lowest category
#       binary exposure             -> the "unexposed / absent" state
#       multi-category nominal      -> the most prevalent category (stability)
#    Change a value here and re-run 06 -> 07 -> render to apply it everywhere.
# ------------------------------------------------------------------------------
PREDICTORS <- tribble(
  ~var_id, ~var,                      ~label,                          ~ref_label,                                         ~biv_section, ~in_panelA, ~in_panelB, ~a_priori, ~ref_rule,
  "V044",  "age_group_3cat",          "Age group",                     "15\u201324 years",                                 "A",          TRUE,       TRUE,       TRUE,      "ordinal: lowest category",
  "V047",  "education_binary",        "Education level",               "Primary or less",                                  "A",          TRUE,       TRUE,       TRUE,      "binary: lower attainment",
  "V046",  "parity_binary",           "Parity",                        "0\u20132 children",                                "A",          TRUE,       TRUE,       TRUE,      "binary: lower parity",
  "V042",  "knowledge_level",         "Knowledge level",               "Low Knowledge (< mean)",                           "A",          TRUE,       TRUE,       TRUE,      "binary: low knowledge",
  "V048",  "partner_support_binary",  "Partner support",               "Not supportive / Don't Know",                      "B",          FALSE,      TRUE,       TRUE,      "binary: not supportive",
  "V049",  "decision_binary",         "Decision making",               "Husband / Other",                                  "B",          FALSE,      TRUE,       TRUE,      "binary: husband/other decides",
  "V035",  "partner_communication",   "Partner communication",         "Never",                                            "B",          FALSE,      TRUE,       TRUE,      "ordinal: lowest category",
  "V050",  "myth_infertility_binary", "Infertility myth",              "Disagree or Don't Know",                           "C",          TRUE,       TRUE,       TRUE,      "binary: myth not endorsed",
  "V051",  "myth_cancer_binary",      "Cancer myth",                   "Disagree or Don't Know",                           "C",          TRUE,       TRUE,       TRUE,      "binary: myth not endorsed",
  "V052",  "myth_infidelity_binary",  "Infidelity myth",               "Disagree or Don't Know",                           "C",          TRUE,       TRUE,       TRUE,      "binary: myth not endorsed",
  "V053",  "cultural_value_binary",   "Cultural value of children",    "Disagree or Don't Know",                           "C",          TRUE,       TRUE,       TRUE,      "binary: not agreeing",
  "V054",  "religious_binary",        "Religious acceptance",          "Not allowed or Don't Know",                        "C",          TRUE,       TRUE,       TRUE,      "binary: not allowed/DK",
  # regression-only predictors (no chi-square in the approved analysis plan)
  "V004",  "religion_model",          "Religion",                      "Muslim",                                           NA,           TRUE,       TRUE,       TRUE,      "nominal: most prevalent category",
  "V005",  "marital_status",          "Marital/cohabitation status",   "Not in union (Single / Divorced / Widowed)",       NA,           TRUE,       FALSE,      TRUE,      "binary: not in union",
  "V007",  "occupation_status",       "Occupation",                    "Self-employed",                                    NA,           TRUE,       TRUE,       TRUE,      "nominal: most prevalent category"
)

# Row order of predictors in Table 4.5 (Blueprint sheet Table4.5_ModifiedPoisson)
TABLE45_ORDER <- c("V044", "V047", "V046", "V042", "V004", "V005", "V007",
                   "V048", "V049", "V035", "V050", "V051", "V052", "V053", "V054")

# Univariable / adjusted predictor lists, exactly as in analysis_plan
PANEL_A_IDS <- c("V044", "V047", "V046", "V042", "V004", "V005", "V007",
                 "V050", "V051", "V052", "V053", "V054")                    # OBJ3-UV1 / MV1
PANEL_B_IDS <- c("V044", "V047", "V046", "V042", "V004", "V007",
                 "V048", "V049", "V035", "V050", "V051", "V052", "V053", "V054")  # OBJ3-UV2 / MV2

# Religion categories "No religion" (n=2) and "Other" (n=1) are too sparse for a
# prevalence-ratio model (one has zero events in the whole sample, so its log-PR
# is not estimable). They are kept separate in descriptive Table 4.1 and merged
# ONLY for the regression variable `religion_model`, as documented in script 01.

# ------------------------------------------------------------------------------
# 6. FORMATTING HELPERS
# ------------------------------------------------------------------------------
EN <- "\u2013"   # en dash
fmt_num  <- function(x, d = 1) formatC(x, format = "f", digits = d)
fmt_pct  <- function(x, d = 1) paste0(formatC(x, format = "f", digits = d), "%")
fmt_npct <- function(n, N, d = 1) {
  ifelse(is.na(N) | N == 0, "\u2014", paste0(n, " (", fmt_pct(100 * n / N, d), ")"))
}
fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "\u2014",
    p < 0.001 ~ "<0.001",
    p < 0.01  ~ formatC(p, format = "f", digits = 3),
    TRUE      ~ formatC(p, format = "f", digits = 3)
  )
}
fmt_pr <- function(est, lo, hi, d = 2) {
  paste0(fmt_num(est, d), " (", fmt_num(lo, d), EN, fmt_num(hi, d), ")")
}
section_banner <- function(txt) {
  cat("\n", strrep("=", 78), "\n", txt, "\n", strrep("=", 78), "\n", sep = "")
}
stop_if <- function(cond, msg) if (isTRUE(cond)) stop(msg, call. = FALSE)
warn_if <- function(cond, msg) if (isTRUE(cond)) warning(msg, call. = FALSE)

load_rds <- function(name) {
  f <- rds_path(name)
  stop_if(!file.exists(f), paste0("Missing checkpoint: ", f, " - run the preceding script first."))
  readRDS(f)
}
save_rds <- function(obj, name) {
  saveRDS(obj, rds_path(name))
  cat("  [saved] results/rds/", name, ".rds\n", sep = "")
  invisible(obj)
}

# Wilson (score) CI for a proportion; Clopper-Pearson reported as a sensitivity check
prop_ci <- function(x, n, conf = 0.95) {
  w <- suppressWarnings(prop.test(x, n, conf.level = conf, correct = FALSE))
  e <- binom.test(x, n, conf.level = conf)
  list(estimate = x / n,
       wilson_lo = unname(w$conf.int[1]), wilson_hi = unname(w$conf.int[2]),
       exact_lo  = unname(e$conf.int[1]), exact_hi  = unname(e$conf.int[2]))
}
