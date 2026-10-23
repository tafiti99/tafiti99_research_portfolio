# ==============================================================================
# 05_Bivariate_Analysis.R
# TAFITI99 | GROUP E | ONE combined Table 4.4 (Sections A, B, C)
# INPUT : results/rds/data_04_analysis_ready.rds
# OUTPUT: results/rds/results_05_bivariate.rds
# ADAPTED FROM Group E.R `analyze_categorical()` - the same logic is kept:
#   complete cases -> observed table -> row % -> expected counts ->
#   sparse rule (any expected < 1 OR > 20% of cells < 5) ->
#   Pearson chi-square (no correction) else Fisher's exact test.
# Additions required by Blueprint v5: effect size (phi / Cramer's V), one overall
# result per predictor, and section/population labels. A Fisher P-value is never
# labelled as a Pearson chi-square result.
# ==============================================================================
source(here::here("Scripts", "00_setup_config.R"), encoding = "UTF-8")
d <- load_rds(RDS$ready)
section_banner("05 BIVARIATE ANALYSIS (Table 4.4)")

OUT <- "current_contraceptive_use"
out_lv <- levels(d[[OUT]])                     # "No (not using)", "Yes (currently using)"

analyze_categorical <- function(data, predictor, outcome = OUT) {
  ad <- data[complete.cases(data[, c(predictor, outcome)]), ]
  ad[[predictor]] <- droplevels(ad[[predictor]])
  tab <- table(ad[[predictor]], ad[[outcome]])
  tab <- tab[, out_lv, drop = FALSE]
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  exp <- chi$expected
  n_lt1 <- sum(exp < 1); n_lt5 <- sum(exp < 5); pct_lt5 <- 100 * n_lt5 / length(exp)
  sparse <- n_lt1 > 0 || pct_lt5 > 20

  if (!sparse) {
    method <- "Pearson chi-square"
    p <- chi$p.value
    stat <- paste0("\u03c7\u00b2(", chi$parameter, ") = ", fmt_num(chi$statistic, 2))
    df <- unname(chi$parameter)
  } else {
    method <- "Fisher's exact test"
    p <- tryCatch(fisher.test(tab, workspace = 2e7)$p.value,
                  error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 100000)$p.value)
    stat <- "Fisher's exact"
    df <- NA_real_
  }
  n <- sum(tab); k <- min(nrow(tab) - 1, ncol(tab) - 1)
  es_val <- sqrt(unname(chi$statistic) / (n * k))        # phi (2x2) or Cramer's V
  es_name <- if (nrow(tab) == 2 && ncol(tab) == 2) "\u03c6" else "Cram\u00e9r's V"

  tibble(level = rownames(tab),
         n_absent = as.integer(tab[, 1]), n_present = as.integer(tab[, 2]),
         total = as.integer(rowSums(tab)),
         pct_absent = 100 * tab[, 1] / rowSums(tab), pct_present = 100 * tab[, 2] / rowSums(tab)) %>%
    mutate(variable = predictor, method = method, statistic = stat, df = df,
           effect_name = es_name, effect_size = es_val, p_value = p,
           n_analysed = n, n_excluded = nrow(data) - n,
           min_expected = min(exp), n_cells_expected_lt5 = n_lt5, pct_cells_expected_lt5 = pct_lt5,
           pearson_chisq = unname(chi$statistic))
}

# Sections (Blueprint Table4.4): A full sample socio-demographic/knowledge,
# B union-only partner factors, C full sample socio-cultural factors
union <- d %>% filter(in_union == 1)
sections <- tribble(
  ~section, ~section_label, ~var_id,
  "A", "SECTION A \u2014 FULL SAMPLE: sociodemographic and knowledge factors", c("V044", "V047", "V046", "V042"),
  "B", "SECTION B \u2014 WOMEN CURRENTLY MARRIED/COHABITING ONLY (V005 = 1): partner-related factors", c("V048", "V049", "V035"),
  "C", "SECTION C \u2014 FULL SAMPLE: socio-cultural factors", c("V050", "V051", "V052", "V053", "V054"))

rows <- list(); on_hold <- list()
for (s in seq_len(nrow(sections))) {
  pop <- if (sections$section[s] == "B") union else d
  for (vid in sections$var_id[[s]]) {
    sp <- PREDICTORS[PREDICTORS$var_id == vid, ]
    if (vid == "V042" && !KNOWLEDGE_KEY_VERIFIED) {
      on_hold[[vid]] <- tibble(section = sections$section[s], var_id = vid, label = sp$label,
                               levels = list(levels(d$knowledge_level)))
      next
    }
    r <- analyze_categorical(pop, sp$var) %>%
      mutate(section = sections$section[s], var_id = vid, label = sp$label,
             population = ifelse(sections$section[s] == "B", "Married/cohabiting women (V005 = 1)", "Full sample"),
             n_eligible_section = nrow(pop))
    rows[[vid]] <- r
    cat(sprintf("%-6s %-26s %-20s %-18s p=%s | N=%d\n", vid, sp$label, unique(r$method),
                unique(r$statistic), fmt_p(unique(r$p_value)), unique(r$n_analysed)))
  }
}
tbl <- bind_rows(rows)

res <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  outcome = OUT, outcome_levels = out_lv,
  sections = sections %>% select(section, section_label),
  section_N = tibble(section = c("A", "B", "C"), n_eligible = c(nrow(d), nrow(union), nrow(d))),
  table = tbl,
  p_by_variable = tbl %>% distinct(var_id, label, section, method, p_value),
  on_hold = bind_rows(on_hold),
  rule = "Pearson chi-square (no continuity correction) unless any expected < 1 or > 20% of cells expected < 5; then Fisher's exact test. Effect size from uncorrected Pearson chi-square: phi (2x2) or Cramer's V.")
save_rds(res, RDS$bivar)
