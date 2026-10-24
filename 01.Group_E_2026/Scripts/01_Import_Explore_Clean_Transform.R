# ==============================================================================
# 01_Import_Explore_Clean_Transform.R
# TAFITI99 | GROUP E | Chapters 6-9 of the book in ONE script
#   import -> explore -> visualise/diagnose -> clean -> transform -> validate
# ------------------------------------------------------------------------------
# ADAPTED FROM Scripts/Group E.R (kept unchanged as the original working script):
#   import + SPSS labels, exploration checks, Chapter 8 plots, Chapter 9 cleaning
#   decisions (drop notes, distinct(), IDs 106/170/229/424 -> 9xxx, skip-pattern NA)
#   and the Chapter 11 derived variables are reused with the SAME logic. Stage 4
#   proves this: the cleaned data are reconciled against data/groupE_cleaned.csv.
# Rule: Explore to discover. Clean to correct. Analyse to answer.
# Every cleaning action follows  Detect -> Document -> Decide -> Transform ->
# Validate -> Audit  and is written to a cleaning log.
#
# INPUTS : data/class_work_2_raw.xlsx           (never overwritten)
#          data/class work 2 - SPSS labels.sps  (value labels, used for reconciliation)
#          resources/TAFITI99_Master_Research_Metadata_v3.xlsx
# OUTPUTS: results/rds/data_01_imported_raw.rds     immutable import
#          results/rds/audit_02_exploration.rds     audit summaries / flags only
#          results/rds/data_03_cleaned_checked.rds  documented cleaning applied
#          results/rds/data_04_analysis_ready.rds   metadata-named, validated variables
#          results/tables/cleaning_log.csv
#          figures/diagnostics/*.png
# ==============================================================================
source(here::here("Scripts", "00_setup_config.R"), encoding = "UTF-8")
set.seed(SEED)
md <- read_metadata()
vmeta <- md$variables
cats  <- md$categories

# ==============================================================================
# STAGE 1 - IMPORT  (Chapter 6)   Save the data unchanged first.
# ==============================================================================
section_banner("STAGE 1: IMPORT")

raw <- read_excel(P$raw_xlsx, guess_max = 5000)
attr(raw, "source_file")  <- "data/class_work_2_raw.xlsx"
attr(raw, "source_md5")   <- unname(tools::md5sum(P$raw_xlsx))
attr(raw, "imported_at")  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

cat("Rows x columns imported :", nrow(raw), "x", ncol(raw), "\n")
cat("Source file MD5         :", attr(raw, "source_md5"), "\n")

# --- Import audit (never trust a successful import) ---------------------------
missing_cols <- setdiff(SOURCE_MAP$dataset_name, names(raw))
stop_if(length(missing_cols) > 0,
        paste("Expected dataset columns not found:", paste(missing_cols, collapse = ", ")))

# dataset <-> metadata consistency: the question-id prefix must agree
# (dataset names are truncated, so only the part before the first '_' is compared)
map_check <- SOURCE_MAP %>%
  left_join(vmeta %>% select(variable_id, variable_name, source_name, question_id), by = "variable_id") %>%
  mutate(ds_prefix = toupper(sub("_.*$", "", dataset_name)),
         md_prefix = toupper(sub("_.*$", "", source_name)),
         prefix_ok = ds_prefix == md_prefix)
stop_if(any(is.na(map_check$variable_name)), "SOURCE_MAP contains a variable_id not found in the metadata.")
stop_if(any(!map_check$prefix_ok),
        paste("Dataset/metadata question-id mismatch:",
              paste(map_check$dataset_name[!map_check$prefix_ok], collapse = ", ")))
cat("Dataset <-> Master Metadata mapping: all", nrow(map_check), "variables agree.\n")

coded_vars <- SOURCE_MAP %>% inner_join(distinct(cats, variable_id), by = "variable_id")
non_numeric <- coded_vars$dataset_name[!vapply(raw[coded_vars$dataset_name], is.numeric, logical(1))]
stop_if(length(non_numeric) > 0,
        paste("Coded variables imported as non-numeric:", paste(non_numeric, collapse = ", ")))
cat("All", nrow(coded_vars), "coded variables imported as numeric codes.\n")

save_rds(raw, RDS$raw)

# ==============================================================================
# STAGE 2 - EXPLORE  (Chapter 7)   Detect and diagnose; do NOT modify the data.
# ==============================================================================
section_banner("STAGE 2: EXPLORATION (detect and document, no changes)")

qn <- raw %>% select(-any_of(ADMIN_COLS))            # questionnaire columns only
cat("Questionnaire columns analysed:", ncol(qn), "\n")

audit <- list()
audit$import <- list(
  n_rows = nrow(raw), n_cols = ncol(raw), n_questionnaire_cols = ncol(qn),
  source_md5 = attr(raw, "source_md5"), mapping_ok = TRUE
)

# 2.1 Missingness profile ------------------------------------------------------
audit$missingness <- tibble(
  dataset_name = names(qn),
  missing_n    = colSums(is.na(qn)),
  missing_pct  = round(100 * colMeans(is.na(qn)), 2)
) %>% filter(missing_n > 0) %>% arrange(desc(missing_n))
cat("\nVariables with missing values:", nrow(audit$missingness), "\n")
print(audit$missingness, n = 30)

# 2.2 Duplicates (Chapter 7.34) -------------------------------------------------
# TYPE 1: exact duplicates (every questionnaire column identical)
exact_dup_rows <- which(duplicated(qn))
# TYPE 2: ID collisions after removing exact duplicates
qn_nodup <- distinct(qn)
id_collisions <- qn_nodup %>% count(ID_NUMBE) %>% filter(n > 1)
audit$duplicates <- list(
  n_exact_duplicate_rows = length(exact_dup_rows),
  exact_duplicate_ids    = qn$ID_NUMBE[exact_dup_rows],
  n_id_collisions        = nrow(id_collisions),
  collision_ids          = id_collisions$ID_NUMBE,
  collision_inspection   = qn_nodup %>%
    filter(ID_NUMBE %in% id_collisions$ID_NUMBE) %>%
    arrange(ID_NUMBE) %>%
    select(ID_NUMBE, QNA1_AGE, QNA2_EDU, QNA3_REL, QNA4_MAR, QNA7_NO, QNC1_CURT)
)
cat("\nExact duplicate rows :", audit$duplicates$n_exact_duplicate_rows,
    "(IDs:", paste(audit$duplicates$exact_duplicate_ids, collapse = ", "), ")\n")
cat("ID collisions        :", audit$duplicates$n_id_collisions,
    "(IDs:", paste(audit$duplicates$collision_ids, collapse = ", "), ")\n")
print(audit$duplicates$collision_inspection, n = 20)

# 2.3 Eligibility and distributions -------------------------------------------
audit$eligibility <- list(
  age_range_required = "15-49",
  n_age_violations   = sum(qn$QNA1_AGE < 15 | qn$QNA1_AGE > 49, na.rm = TRUE),
  n_age_missing      = sum(is.na(qn$QNA1_AGE))
)
skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / (mean((x - mean(x))^2)^1.5) }
audit$distributions <- tibble(
  variable = c("age", "living_children"),
  mean = c(mean(qn$QNA1_AGE, na.rm = TRUE), mean(qn$QNA7_NO, na.rm = TRUE)),
  median = c(median(qn$QNA1_AGE, na.rm = TRUE), median(qn$QNA7_NO, na.rm = TRUE)),
  min = c(min(qn$QNA1_AGE, na.rm = TRUE), min(qn$QNA7_NO, na.rm = TRUE)),
  max = c(max(qn$QNA1_AGE, na.rm = TRUE), max(qn$QNA7_NO, na.rm = TRUE)),
  skewness = c(skew(qn$QNA1_AGE), skew(qn$QNA7_NO))
)
cat("\nAge eligibility violations:", audit$eligibility$n_age_violations, "\n")
print(audit$distributions)

# Age heaping (digit preference)
age_last <- table(qn$QNA1_AGE %% 10)
audit$age_heaping <- list(
  last_digit_table = age_last,
  pct_ending_0_or_5 = round(100 * sum(age_last[c("0", "5")], na.rm = TRUE) / sum(age_last), 1),
  expected_if_uniform = 20
)
cat("Ages ending in 0 or 5:", audit$age_heaping$pct_ending_0_or_5, "% (uniform expectation 20%)\n")

# 2.4 Code validity against Master Metadata categories --------------------------
code_validity <- map_dfr(seq_len(nrow(coded_vars)), function(i) {
  ds <- coded_vars$dataset_name[i]; vid <- coded_vars$variable_id[i]
  valid <- cats$code[cats$variable_id == vid]
  x <- qn[[ds]]
  tibble(dataset_name = ds, variable_id = vid,
         n_invalid = sum(!is.na(x) & !(x %in% valid)),
         invalid_codes = paste(sort(unique(x[!is.na(x) & !(x %in% valid)])), collapse = ","))
})
audit$code_validity <- code_validity
cat("\nCoded variables with out-of-range codes:", sum(code_validity$n_invalid > 0), "\n")

# 2.5 Skip-pattern consistency (Metadata rules R004-R012) ------------------------
skip_rules <- list(
  list(rule = "R004/R005", var = "QNA5_POL",  desc = "Polygamy asked only if in union (QNA4_MAR = 1)",
       applies = function(d) d$QNA4_MAR %in% 1),
  list(rule = "R006/R007", var = "QNA8_CHI",  desc = "Child spacing asked only if living children >= 2",
       applies = function(d) d$QNA7_NO >= 2 & !is.na(d$QNA7_NO)),
  list(rule = "R008/R009", var = "QNC2_CON",  desc = "Method asked only if currently using (QNC1_CURT = 2)",
       applies = function(d) d$QNC1_CURT %in% 2),
  list(rule = "R010",      var = "QND1_HUSH", desc = "Partner support asked only if in union",
       applies = function(d) d$QNA4_MAR %in% 1),
  list(rule = "R011",      var = "QND2_DEC4", desc = "Decision-maker asked only if in union",
       applies = function(d) d$QNA4_MAR %in% 1),
  list(rule = "R012",      var = "QND3_COM",  desc = "Partner communication asked only if in union",
       applies = function(d) d$QNA4_MAR %in% 1)
)
skip_check <- function(d) {
  map_dfr(skip_rules, function(r) {
    ap <- r$applies(d); x <- d[[r$var]]
    tibble(rule = r$rule, variable = r$var, description = r$desc,
           n_applicable = sum(ap), n_not_applicable = sum(!ap),
           answered_when_should_skip = sum(!ap & !is.na(x)),
           missing_when_should_ask   = sum(ap & is.na(x)))
  })
}
audit$skip_checks <- skip_check(qn_nodup)
cat("\nSkip-pattern consistency (after removing exact duplicates):\n")
print(as.data.frame(audit$skip_checks[, c("rule", "variable", "answered_when_should_skip", "missing_when_should_ask")]))

# 2.6 Biological plausibility (age vs parity) -----------------------------------
audit$bio_logic <- qn_nodup %>%
  filter((QNA1_AGE < 18 & QNA7_NO >= 3) | (QNA1_AGE < 20 & QNA7_NO >= 4)) %>%
  select(ID_NUMBE, QNA1_AGE, QNA7_NO)
cat("\nBiologically implausible age/parity combinations:", nrow(audit$bio_logic), "\n")

# 2.7 Knowledge item completeness ------------------------------------------------
kn_ds <- SOURCE_MAP$dataset_name[match(KNOWLEDGE_IDS, SOURCE_MAP$variable_id)]
audit$knowledge_missing <- tibble(
  variable_id = KNOWLEDGE_IDS, dataset_name = kn_ds,
  missing_n = colSums(is.na(qn_nodup[kn_ds])))
cat("Respondents with any missing knowledge item:",
    sum(rowSums(is.na(qn_nodup[kn_ds])) > 0), "\n")

# 2.8 Outcome stratification (informational) -------------------------------------
audit$outcome_by_marital <- table(marital = qn_nodup$QNA4_MAR, using = qn_nodup$QNC1_CURT, useNA = "ifany")
audit$outcome_missing <- sum(is.na(qn_nodup$QNC1_CURT))
audit$religion_sparsity <- table(religion = qn_nodup$QNA3_REL, using = qn_nodup$QNC1_CURT)
cat("\nOutcome (QNC1_CURT) missing:", audit$outcome_missing, "\n")
cat("Religion x outcome (sparse categories matter for modelling):\n"); print(audit$religion_sparsity)

save_rds(audit, RDS$audit)

# ==============================================================================
# STAGE 3 - VISUAL DIAGNOSTICS  (Chapter 8)
# ==============================================================================
section_banner("STAGE 3: VISUAL DIAGNOSTICS")

if (requireNamespace("naniar", quietly = TRUE)) {
  p_miss <- naniar::vis_miss(qn_nodup %>% select(-any_of(c("QNB2I_SPECIFY", "QNB3E_SPECIFY")))) +
    labs(title = "Missingness map (blocks = skip-pattern missingness)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
  ggsave(file.path(P$fig_diag, "diag_missingness.png"), p_miss, width = 10, height = 6, dpi = 200)
}
p_age <- ggplot(qn_nodup, aes(QNA1_AGE)) +
  geom_histogram(binwidth = 1, fill = "grey55", colour = "white") +
  labs(title = "Age, 1-year bins (check heaping)", x = "Age (years)", y = "Women") +
  theme_minimal()
ggsave(file.path(P$fig_diag, "diag_age_histogram.png"), p_age, width = 7, height = 4, dpi = 200)
p_par <- ggplot(qn_nodup, aes(factor(QNA7_NO))) +
  geom_bar(fill = "grey55") +
  labs(title = "Living children (check implausible tail)", x = "Number of living children", y = "Women") +
  theme_minimal()
ggsave(file.path(P$fig_diag, "diag_parity_bar.png"), p_par, width = 7, height = 4, dpi = 200)
cat("Diagnostic figures written to figures/diagnostics/\n")

# ==============================================================================
# STAGE 4 - CLEAN  (Chapter 9)   Detect -> Document -> Decide -> Transform ->
#                                  Validate -> Audit
# Decisions below are the documented decisions of the Group E team
# (inherited from Group E.R); every one is logged with the number of records hit.
# ==============================================================================
section_banner("STAGE 4: CLEANING (documented decisions)")

cleaning_log <- tibble(step = character(), finding = character(), decision = character(),
                       n_affected = integer())
log_step <- function(step, finding, decision, n) {
  cleaning_log <<- bind_rows(cleaning_log, tibble(step = step, finding = finding,
                                                  decision = decision, n_affected = as.integer(n)))
}

dw <- raw %>% select(-any_of(ADMIN_COLS))            # protect raw: work on a copy
n_start <- nrow(dw)
log_step("C0", "Raw KoboToolbox export imported", "Working copy created; raw RDS untouched", n_start)

# C1 - structural cleaning ------------------------------------------------------
log_step("C1", "Kobo administrative / note columns (enumerator_id, note_*, _id, _uuid, ...) are not study data",
         "Dropped from the working copy (kept in data_01 raw RDS)", length(intersect(names(raw), ADMIN_COLS)))

# C2 - exact duplicates ------------------------------------------------------------
n_exact <- sum(duplicated(dw))
dw <- distinct(dw)
log_step("C2", "Rows identical in every questionnaire column (double submission)",
         "Removed; first occurrence kept", n_exact)

# C3 - ID collisions: different women recorded under the same ID --------------------
id_fix <- tribble(
  ~id,  ~match_desc,         ~new_id,
  106,  "age == 15",         9106,
  170,  "age == 26",         9170,
  229,  "age == 19",         9229,
  424,  "second occurrence", 9424
)
hit <- c(sum(dw$ID_NUMBE == 106 & dw$QNA1_AGE == 15), sum(dw$ID_NUMBE == 170 & dw$QNA1_AGE == 26),
         sum(dw$ID_NUMBE == 229 & dw$QNA1_AGE == 19), sum(dw$ID_NUMBE == 424) - 1)
stop_if(any(hit != 1), "ID-collision rule did not match exactly one record each; re-inspect audit$duplicates.")
dup_second <- duplicated(dw$ID_NUMBE)
dw <- dw %>% mutate(
  ID_NUMBE = case_when(
    ID_NUMBE == 106 & QNA1_AGE == 15 ~ 9106,
    ID_NUMBE == 170 & QNA1_AGE == 26 ~ 9170,
    ID_NUMBE == 229 & QNA1_AGE == 19 ~ 9229,
    ID_NUMBE == 424 & dup_second     ~ 9424,
    TRUE ~ ID_NUMBE))
log_step("C3", "IDs 106, 170, 229 and 424 each shared by two different women (conflicting answers)",
         "NOT deleted; second woman re-labelled 9106, 9170, 9229, 9424 (paper-form verification by team)", sum(hit))

# C4 - skip-pattern violations -----------------------------------------------------
viol_before <- skip_check(dw)
for (r in skip_rules) {
  ap <- r$applies(dw)
  dw[[r$var]][!ap] <- NA
}
log_step("C4", "Women outside a skip condition holding answers (e.g. not in union with partner answers)",
         "Set to NA to respect the questionnaire protocol (R004-R012)",
         sum(viol_before$answered_when_should_skip))

# C5 - user-missing code for polygamy (R001) -----------------------------------------
n_code9 <- sum(dw$QNA5_POL %in% 9)
dw$QNA5_POL[dw$QNA5_POL %in% 9] <- NA
log_step("C5", "Polygamy user-missing code 9 (R001)", "Converted to NA", n_code9)

# C6 - eligibility (R002) and living children (R003): flag, do not delete -----------
n_age_bad <- sum(dw$QNA1_AGE < 15 | dw$QNA1_AGE > 49, na.rm = TRUE)
n_par_bad <- sum(dw$QNA7_NO < 0 | dw$QNA7_NO != round(dw$QNA7_NO), na.rm = TRUE)
log_step("C6", "Age outside 15-49 / non-integer or negative living children", "None found; no records excluded",
         n_age_bad + n_par_bad)

# --- Validation --------------------------------------------------------------------
cat("\nVALIDATION\n")
v <- list(
  ids_unique        = !any(duplicated(dw$ID_NUMBE)),
  exact_dups_left   = sum(duplicated(dw)),
  skip_violations   = sum(skip_check(dw)$answered_when_should_skip),
  skip_missing      = sum(skip_check(dw)$missing_when_should_ask),
  age_eligible      = n_age_bad == 0,
  outcome_complete  = !anyNA(dw$QNC1_CURT),
  codes_valid       = all(vapply(seq_len(nrow(coded_vars)), function(i) {
    x <- dw[[coded_vars$dataset_name[i]]]
    all(is.na(x) | x %in% cats$code[cats$variable_id == coded_vars$variable_id[i]])
  }, logical(1)))
)
print(unlist(v))
stop_if(!v$ids_unique, "Duplicate IDs remain after cleaning.")
stop_if(v$exact_dups_left > 0, "Exact duplicates remain after cleaning.")
stop_if(v$skip_violations > 0, "Skip-pattern violations remain after cleaning.")
stop_if(!v$outcome_complete, "Outcome has missing values; denominators must be reviewed.")
stop_if(!v$codes_valid, "Out-of-range codes remain after cleaning.")
warn_if(v$skip_missing > 0, paste(v$skip_missing, "applicable item(s) missing (not skip-related); see audit."))
cat("Rows: start", n_start, "-> after cleaning", nrow(dw), "(", n_start - nrow(dw), "removed )\n")
log_step("C7", "Validation of cleaned file", paste0("Final N = ", nrow(dw)), nrow(dw))

# --- Reconciliation with the earlier Group E.R output (informational) -----------------
parse_sps_labels <- function(path) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  i <- which(grepl("^VALUE LABELS", x))[1]
  out <- list(); cur <- NULL
  for (ln in x[(i + 1):length(x)]) {
    t <- str_trim(ln)
    if (t == "" || t == ".") next
    m <- str_match(t, "^'([^']*)'\\s+'(.*)'$")
    if (!is.na(m[1, 1])) {
      if (!is.null(cur)) out[[cur]][[m[1, 2]]] <- str_replace_all(m[1, 3], "''", "'")
    } else { cur <- str_trim(sub("^/", "", t)); out[[cur]] <- list() }
  }
  out
}
sps_lab <- parse_sps_labels(P$sps)
recon <- NA_character_
if (file.exists(P$legacy_csv)) {
  leg <- read_csv(P$legacy_csv, show_col_types = FALSE, guess_max = 5000)
  ours <- dw
  for (nm in intersect(names(sps_lab), names(ours)))
    if (!nm %in% c("enumerator_id")) ours[[nm]] <- unname(unlist(sps_lab[[nm]])[as.character(ours[[nm]])])
  ours <- ours %>% mutate(across(where(is.character), as.character))
  leg  <- leg  %>% mutate(across(where(is.character), as.character))
  common <- intersect(names(leg), names(ours))
  a <- ours[order(ours$ID_NUMBE), common]; b <- leg[order(leg$ID_NUMBE), common]
  same <- nrow(a) == nrow(b) && isTRUE(all.equal(as.data.frame(a), as.data.frame(b),
                                                  check.attributes = FALSE))
  recon <- if (same) "IDENTICAL to data/groupE_cleaned.csv from Group E.R" else
    "DIFFERS from data/groupE_cleaned.csv (review)"
  cat("\nReconciliation with legacy cleaned CSV:", recon, "\n")
  warn_if(!same, "Cleaned data differ from the legacy groupE_cleaned.csv; review before analysis.")
}
log_step("C8", "Reconciliation with legacy Group E.R cleaned CSV", recon, nrow(dw))

attr(dw, "cleaning_log") <- cleaning_log
write_csv(cleaning_log, file.path(P$tables, "cleaning_log.csv"))
print(as.data.frame(cleaning_log[, c("step", "decision", "n_affected")]))
save_rds(dw, RDS$cleaned)

# ==============================================================================
# STAGE 5 - TRANSFORM  (Chapters 6-9 + Master Metadata derivations)
# Canonical metadata names; factor labels come from Master Metadata `categories`.
# ==============================================================================
section_banner("STAGE 5: TRANSFORM TO ANALYSIS-READY DATA")

lab_of <- function(vid) {
  cc <- cats %>% filter(variable_id == vid) %>% arrange(code)
  list(codes = cc$code, labels = cc$label)
}
to_factor <- function(x, vid) {
  l <- lab_of(vid); factor(x, levels = l$codes, labels = l$labels)
}

ar <- tibble(.rows = nrow(dw))
for (i in seq_len(nrow(SOURCE_MAP))) {
  ds  <- SOURCE_MAP$dataset_name[i]; vid <- SOURCE_MAP$variable_id[i]
  nm  <- vmeta$variable_name[match(vid, vmeta$variable_id)]
  ar[[nm]] <- if (vid %in% cats$variable_id) to_factor(dw[[ds]], vid) else dw[[ds]]
}
ar$participant_id <- as.integer(ar$participant_id)
ar$age            <- as.integer(ar$age)
ar$living_children <- as.integer(ar$living_children)

# Regression religion variable: merge ONLY the two sparse categories (documented)
ar$religion_model <- factor(
  case_when(dw$QNA3_REL == 1 ~ "Christian", dw$QNA3_REL == 2 ~ "Muslim",
            dw$QNA3_REL %in% c(3, 4) ~ "No religion / Other"),
  levels = c("Christian", "Muslim", "No religion / Other"))

# --- Binary outcome (V031: code 2 = currently using) ---------------------------------
ar$outcome_use <- as.integer(dw$QNC1_CURT == 2)
ar$in_union    <- as.integer(dw$QNA4_MAR == 1)

# --- Derived variables V043-V054 (Master Metadata derivations) -----------------------
age <- dw$QNA1_AGE; par <- dw$QNA7_NO
ar$age_group <- to_factor(as.integer(cut(age, c(-Inf, 19, 24, 29, 34, 39, 44, Inf), labels = FALSE)), "V043")
ar$age_group_3cat <- to_factor(as.integer(cut(age, c(-Inf, 24, 34, Inf), labels = FALSE)), "V044")
ar$parity_group <- to_factor(case_when(par == 0 ~ 1L, par <= 2 ~ 2L, par <= 4 ~ 3L, par >= 5 ~ 4L), "V045")
ar$parity_binary <- to_factor(case_when(par <= 2 ~ 1L, par >= 3 ~ 2L), "V046")
ar$education_binary <- to_factor(case_when(dw$QNA2_EDU %in% c(0, 1) ~ 1L, dw$QNA2_EDU %in% c(2, 3) ~ 2L), "V047")
ar$partner_support_binary <- to_factor(case_when(dw$QND1_HUSH == 1 ~ 1L, dw$QND1_HUSH %in% c(2, 3) ~ 2L), "V048")
ar$decision_binary <- to_factor(case_when(dw$QND2_DEC4 %in% c(1, 3) ~ 1L, dw$QND2_DEC4 %in% c(2, 4) ~ 2L), "V049")
bin_agree <- function(code) case_when(code == 2 ~ 1L, code %in% c(1, 3) ~ 2L)   # Yes=agree vs No/DK
ar$myth_infertility_binary <- to_factor(bin_agree(dw$QND4_MYTA), "V050")
ar$myth_cancer_binary      <- to_factor(bin_agree(dw$QND5_MYT2), "V051")
ar$myth_infidelity_binary  <- to_factor(bin_agree(dw$QND6_MYTG), "V052")
ar$cultural_value_binary   <- to_factor(bin_agree(dw$QND7_MANT), "V053")
ar$religious_binary        <- to_factor(bin_agree(dw$QND8_REL3), "V054")

# --- V041 / V042: composite knowledge score and knowledge level ----------------------
items <- dw[kn_ds]
ar$knowledge_score <- ifelse(rowSums(is.na(items)) > 0, NA_real_, rowSums(items == 1))
mu <- mean(ar$knowledge_score, na.rm = TRUE)
ar$knowledge_level <- to_factor(case_when(ar$knowledge_score >= mu ~ 2L, ar$knowledge_score < mu ~ 1L), "V042")
cat("Knowledge score derived (V041/V042). Mean =", round(mu, 3), "\n")
attr(ar, "religion_model_note") <- "'No religion' (n=2) and 'Other' (n=1) merged for regression only"

# --- Validation of the derivations ---------------------------------------------------
cat("\nTRANSFORM VALIDATION\n")
union_only <- c("polygamy", "partner_support", "decision_maker", "partner_communication",
                "partner_support_binary", "decision_binary")
chk <- list(
  n_rows_equal_cleaned   = nrow(ar) == nrow(dw),
  outcome_no_na          = !anyNA(ar$outcome_use),
  age_group_3cat_no_na   = !anyNA(ar$age_group_3cat),
  age_group_sums         = sum(table(ar$age_group)) == nrow(ar),
  parity_group_sums      = sum(table(ar$parity_group)) == nrow(ar),
  education_binary_no_na = !anyNA(ar$education_binary),
  myths_no_na            = !anyNA(ar$myth_infertility_binary) & !anyNA(ar$myth_cancer_binary) &
    !anyNA(ar$myth_infidelity_binary) & !anyNA(ar$cultural_value_binary) & !anyNA(ar$religious_binary),
  union_vars_na_iff_not_in_union = all(vapply(union_only, function(v) all(is.na(ar[[v]]) == (ar$in_union == 0)), logical(1))),
  child_spacing_na_iff_lt2       = all(is.na(ar$child_spacing) == (ar$living_children < 2)),
  method_na_iff_non_user         = all(is.na(ar$current_contraceptive_method) == (ar$outcome_use == 0)),
  labels_match_metadata_V044     = identical(levels(ar$age_group_3cat), lab_of("V044")$labels),
  religion_model_no_na           = !anyNA(ar$religion_model),
  knowledge_complete             = !anyNA(ar$knowledge_score) & !anyNA(ar$knowledge_level)
)
print(unlist(chk))
stop_if(!all(unlist(chk)), "Transform validation failed - see list above.")

cat("\nAnalysis-ready N =", nrow(ar), "| events (current users) =", sum(ar$outcome_use),
    "| in union =", sum(ar$in_union), "\n")

# Drop identifier-free helper only; keep a tidy column order
ar <- ar %>% relocate(participant_id, age, age_group, age_group_3cat, education, education_binary,
                      religion, religion_model, marital_status, in_union, occupation_status,
                      living_children, parity_group, parity_binary, child_spacing, polygamy)
save_rds(ar, RDS$ready)

cat("\n01_Import_Explore_Clean_Transform.R completed.\n")
