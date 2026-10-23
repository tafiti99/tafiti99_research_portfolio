# ==============================================================================
# 04_Descriptive_Analysis.R
# TAFITI99 | GROUP E | Tables 4.1, 4.2, 4.3 (descriptive results)
# INPUT : results/rds/data_04_analysis_ready.rds
# OUTPUT: results/rds/results_04_descriptive.rds
# Adapted from Group E.R (variable derivations, prevalence of QNC1_CURT) and the
# Chapter 20 shells. Percentages use the variable-specific valid denominator;
# missing / not-applicable counts are stored for every variable.
# ==============================================================================
source(here::here("Scripts", "00_setup_config.R"), encoding = "UTF-8")
d  <- load_rds(RDS$ready)
md <- read_metadata()
N  <- nrow(d)
section_banner("04 DESCRIPTIVE ANALYSIS")
cat("Analysis-ready N =", N, "\n")

skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / (mean((x - mean(x))^2)^1.5) }

# --- Continuous summaries with documented mean(SD) / median(Q1-Q3) decision -------
cont_summary <- function(x, label) {
  v <- x[!is.na(x)]
  sk <- skew(v)
  use_mean <- abs(sk) < SKEW_RULE
  q <- quantile(v, c(.25, .75), type = 7)
  tibble(label = label, n_valid = length(v), n_missing = sum(is.na(x)),
         mean = mean(v), sd = sd(v), median = median(v), q1 = q[[1]], q3 = q[[2]],
         min = min(v), max = max(v), skewness = sk,
         shapiro_p = tryCatch(shapiro.test(v)$p.value, error = function(e) NA_real_),
         statistic_used = ifelse(use_mean, "mean (SD)", "median (Q1\u2013Q3)"),
         display = ifelse(use_mean,
                          paste0(fmt_num(mean(v), 1), " (", fmt_num(sd(v), 1), ")"),
                          paste0(fmt_num(median(v), 0), " (", fmt_num(q[[1]], 0), EN, fmt_num(q[[2]], 0), ")")))
}
continuous <- bind_rows(
  cont_summary(d$age, "Age, years"),
  cont_summary(d$living_children, "Living children, number"))
print(continuous %>% select(label, n_valid, n_missing, skewness, statistic_used, display))

# --- Categorical blocks (Table 4.1) ---------------------------------------------------
# applicability (structural missingness) is explained, not hidden
cat_block <- function(var, label, note = NA_character_) {
  x <- d[[var]]
  lv <- levels(x); tb <- table(x, useNA = "no"); nv <- sum(tb)
  tibble(var = var, label = label, level = lv, n = as.integer(tb[lv]),
         pct = 100 * as.integer(tb[lv]) / nv, n_valid = nv, n_missing = sum(is.na(x)),
         note = note)
}
categorical <- bind_rows(
  cat_block("age_group", "Age group, n (%)"),
  cat_block("education", "Educational attainment, n (%)"),
  cat_block("religion", "Religion, n (%)"),
  cat_block("marital_status", "Marital/cohabitation status, n (%)"),
  cat_block("occupation_status", "Occupation, n (%)"),
  cat_block("parity_group", "Parity group, n (%)"),
  cat_block("child_spacing", "Child spacing*, n (%)",
            "Asked only if living children >= 2 (skip logic); missing = not applicable"),
  cat_block("polygamy", "Polygamy**, n (%)",
            "Asked only if currently married/cohabiting (skip logic); missing = not applicable"))

# structural-missingness verification for the footnotes
applicability <- list(
  child_spacing_eligible = sum(d$living_children >= 2),
  child_spacing_not_applicable = sum(d$living_children < 2),
  child_spacing_missing_among_eligible = sum(d$living_children >= 2 & is.na(d$child_spacing)),
  polygamy_eligible = sum(d$in_union == 1),
  polygamy_not_applicable = sum(d$in_union == 0),
  polygamy_missing_among_eligible = sum(d$in_union == 1 & is.na(d$polygamy)))
print(applicability)

# --- Table 4.2: outcome distribution and prevalence --------------------------------
y <- d$outcome_use
n_out <- sum(!is.na(y)); n_use <- sum(y == 1, na.rm = TRUE); n_not <- sum(y == 0, na.rm = TRUE)
ci <- prop_ci(n_use, n_out)
prevalence <- list(
  outcome_variable = "V031 current_contraceptive_use",
  n_total_records = N, n_with_outcome = n_out, n_excluded_missing_outcome = N - n_out,
  n_using = n_use, n_not_using = n_not,
  pct_using = 100 * n_use / n_out, pct_not_using = 100 * n_not / n_out,
  ci_method = PREV_CI_METHOD,
  ci_lo = 100 * ci$wilson_lo, ci_hi = 100 * ci$wilson_hi,
  exact_ci_lo = 100 * ci$exact_lo, exact_ci_hi = 100 * ci$exact_hi,
  in_union = list(n = sum(d$in_union == 1), n_using = sum(y[d$in_union == 1]),
                  pct = 100 * mean(y[d$in_union == 1]),
                  ci = unlist(prop_ci(sum(y[d$in_union == 1]), sum(d$in_union == 1))[c("wilson_lo", "wilson_hi")]) * 100),
  methods_among_users = d %>% filter(outcome_use == 1) %>%
    count(current_contraceptive_method) %>% mutate(pct = 100 * n / sum(n)))
cat(sprintf("\nPrevalence: %d/%d = %.1f%% (%s 95%% CI %.1f-%.1f; Clopper-Pearson %.1f-%.1f)\n",
            n_use, n_out, prevalence$pct_using, PREV_CI_METHOD, prevalence$ci_lo, prevalence$ci_hi,
            prevalence$exact_ci_lo, prevalence$exact_ci_hi))

# --- Table 4.3: knowledge items ----------------------------------------------------------
vm <- md$variables
kn_domain <- tibble(variable_id = KNOWLEDGE_IDS,
                    domain = c(rep("Knowledge of modern contraceptive methods", 6),
                               rep("Knowledge of advantages", 8),
                               rep("Knowledge of side effects", 4)),
                    item = c("Pills", "Injection", "Condom", "Implants (Norplant)", "IUD", "Sterilization",
                             "Prevent unwanted pregnancy", "Aid child spacing", "Allow freedom to enjoy sex",
                             "Help prevent diseases", "Easy to use", "Create confidence", "Reduce birth rate",
                             "More effective than traditional methods",
                             "Method failure (unwanted pregnancy)", "Irregular bleeding",
                             "Stress and fear", "Weight gain"))
knowledge_items <- map_dfr(seq_len(nrow(kn_domain)), function(i) {
  vid <- kn_domain$variable_id[i]; nm <- vm$variable_name[match(vid, vm$variable_id)]
  x <- d[[nm]]; nv <- sum(!is.na(x))
  yes <- levels(x)[1]; no <- levels(x)[2]
  tibble(variable_id = vid, variable = nm, domain = kn_domain$domain[i], item = kn_domain$item[i],
         n_yes = sum(x == yes, na.rm = TRUE), n_no = sum(x == no, na.rm = TRUE),
         n_valid = nv, n_missing = sum(is.na(x)),
         pct_yes = 100 * sum(x == yes, na.rm = TRUE) / nv, pct_no = 100 * sum(x == no, na.rm = TRUE) / nv)
})
knowledge_composite <- list(
  hold = !KNOWLEDGE_KEY_VERIFIED,
  reason = "Correct-response key and missing-item rule not yet verified (Master Metadata critical confirmation 2).",
  score = NULL, level = NULL)
if (KNOWLEDGE_KEY_VERIFIED) {
  knowledge_composite$score <- cont_summary(d$knowledge_score, "Knowledge score (0\u201318)")
  knowledge_composite$level <- cat_block("knowledge_level", "Knowledge level")
}
print(knowledge_items %>% select(variable_id, item, n_yes, n_valid, n_missing, pct_yes), n = 18)

# --- Save -------------------------------------------------------------------------------
res <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  N = N, continuous = continuous, categorical = categorical, applicability = applicability,
  prevalence = prevalence, knowledge_items = knowledge_items, knowledge_composite = knowledge_composite)
save_rds(res, RDS$descr)
