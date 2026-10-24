# ==============================================================================
# 07_Tables_Figures.R
# TAFITI99 | GROUP E | Verified publication-table objects and figure data
# INPUT : results_04_descriptive, results_05_bivariate, results_06c_..._cPR_aPR,
#         data checkpoints (flow counts)
# OUTPUT: results/rds/results_07_table_objects.rds   (Tables 4.1-4.5)
#         results/rds/results_07_figure_data.rds     (Figure 4.1 flow, Figure 4.2 forest)
# No model fitting or recoding happens here: values are only formatted and arranged.
# Row types: section | var | level   (the Quarto layer styles them)
# ==============================================================================
source(here::here("Scripts", "00_setup_config.R"), encoding = "UTF-8")
raw  <- load_rds(RDS$raw)
cln  <- load_rds(RDS$cleaned)
ar   <- load_rds(RDS$ready)
ds   <- load_rds(RDS$descr)
bv   <- load_rds(RDS$bivar)
mp   <- load_rds(RDS$mp_all)
section_banner("07 TABLE OBJECTS AND FIGURE DATA")

N <- nrow(ar)

# ------------------------------------------------------------------------------
# TABLE 4.1  Sociodemographic and other characteristics (one Overall column)
# ------------------------------------------------------------------------------
cont <- ds$continuous; cat_d <- ds$categorical
tag_n <- function(n_valid, n_missing, extra = NULL) {
  if (n_missing == 0) return("")
  paste0(" [valid n = ", n_valid, "; ", ifelse(is.null(extra), "missing", extra), " = ", n_missing, "]")
}
cat_rows <- function(var, header_text, na_word = "missing") {
  b <- cat_d[cat_d$var == var, ]
  bind_rows(
    tibble(row_type = "var", c1 = paste0(header_text, tag_n(b$n_valid[1], b$n_missing[1], na_word)), c2 = ""),
    tibble(row_type = "level", c1 = b$level, c2 = fmt_npct(b$n, b$n_valid)))
}
cont_row <- function(i, text) {
  tibble(row_type = "var", c1 = paste0(text, ", ", cont$statistic_used[i]), c2 = cont$display[i])
}
t41 <- bind_rows(
  cont_row(1, "Age, years"),
  cat_rows("age_group", "Age group, n (%)"),
  cat_rows("education", "Educational attainment, n (%)"),
  cat_rows("religion", "Religion, n (%)"),
  cat_rows("marital_status", "Marital/cohabitation status, n (%)"),
  cat_rows("occupation_status", "Occupation, n (%)"),
  cont_row(2, "Living children, number"),
  cat_rows("parity_group", "Parity group, n (%)"),
  cat_rows("child_spacing", "Child spacing*, n (%)", "not applicable"),
  cat_rows("polygamy", "Polygamy**, n (%)", "not applicable"))
a <- ds$applicability
tbl41 <- list(
  id = "t41", header = c("Characteristic", paste0("Overall, N = ", N)), data = t41,
  widths = c(4.4, 2.1),
  notes = c(
    "Values are n (%) for categorical variables, using the valid (non-missing, applicable) denominator for each variable, and mean (SD) or median (Q1\u2013Q3) for continuous variables.",
    paste0("* Child spacing was asked only of respondents with at least two living children (eligible n = ", a$child_spacing_eligible,
           "; not applicable n = ", a$child_spacing_not_applicable, "; missing among eligible = ", a$child_spacing_missing_among_eligible, ")."),
    paste0("** Polygamy was asked only of currently married/cohabiting respondents (eligible n = ", a$polygamy_eligible,
           "; not applicable n = ", a$polygamy_not_applicable, "; missing among eligible = ", a$polygamy_missing_among_eligible, ").")))

# ------------------------------------------------------------------------------
# TABLE 4.2  Distribution and prevalence of current modern contraceptive use
# ------------------------------------------------------------------------------
pv <- ds$prevalence
t42 <- tibble(
  row_type = c("level", "level", "level", "var"),
  c1 = c("Currently using a modern contraceptive method (outcome present)",
         "Not currently using a modern contraceptive method (outcome absent)",
         "Total with available outcome data",
         "Prevalence of current modern contraceptive use (95% CI)"),
  c2 = c(fmt_npct(pv$n_using, pv$n_with_outcome), fmt_npct(pv$n_not_using, pv$n_with_outcome),
         paste0(pv$n_with_outcome, " (100.0%)"),
         paste0(fmt_pct(pv$pct_using), " (", fmt_num(pv$ci_lo, 1), EN, fmt_num(pv$ci_hi, 1), ")")))
tbl42 <- list(
  id = "t42", header = c("Primary outcome", "Number, n (%) / estimate"), data = t42, widths = c(4.4, 2.1),
  notes = c(
    "Outcome: current use of any modern contraceptive method. Prevalence = number currently using / number with available outcome data \u00d7 100.",
    paste0("The 95% confidence interval was calculated with the ", PREV_CI_METHOD, " (exact Clopper\u2013Pearson interval: ",
           fmt_num(pv$exact_ci_lo, 1), EN, fmt_num(pv$exact_ci_hi, 1), "%)."),
    paste0("Records with missing outcome data excluded from the denominator: ", pv$n_excluded_missing_outcome, " of ", pv$n_total_records, ".")))

# ------------------------------------------------------------------------------
# TABLE 4.3  Knowledge items, composite score and knowledge level
# ------------------------------------------------------------------------------
ki <- ds$knowledge_items
kc <- ds$knowledge_composite
lv <- kc$level
sc <- kc$score
sc_disp <- paste0(fmt_num(sc$mean, 2), " (", fmt_num(sc$sd, 2), ") [median ",
                  fmt_num(sc$median, 0), ", Q1\u2013Q3 ", fmt_num(sc$q1, 0), EN, fmt_num(sc$q3, 0),
                  "; range ", fmt_num(sc$min, 0), EN, fmt_num(sc$max, 0), "]")
t43 <- bind_rows(
  ki %>% transmute(
    domain, item,
    yes = fmt_npct(n_yes, n_valid), no = fmt_npct(n_no, n_valid),
    valid = paste0(n_valid, "; ", n_missing)),
  tibble(
    domain = c("Composite knowledge score", "Knowledge level", "Knowledge level"),
    item = c(paste0("Knowledge score (0\u201318), ", sc$statistic_used),
             "Low knowledge (< mean)", "High knowledge (\u2265 mean)"),
    yes = c(sc_disp, fmt_npct(lv$n[1], lv$n_valid[1]), fmt_npct(lv$n[2], lv$n_valid[2])),
    no = "\u2014",
    valid = paste0(sc$n_valid, "; ", sc$n_missing)))
tbl43 <- list(
  id = "t43",
  header = c("Knowledge domain", "Knowledge item / summary", "Yes, n (%) / summary", "No, n (%)", "Valid n; missing n"),
  data = t43, widths = c(1.55, 2.15, 1.15, 0.95, 0.70), group_col = "domain",
  notes = c(
    "Each item uses its own available-response denominator across the 18 prespecified knowledge items.",
    paste0("Composite knowledge score = sum of 'Yes' responses across the 18 items (range 0\u201318; 0 missing items). Knowledge level dichotomises participants at the sample mean (",
           fmt_num(sc$mean, 2), "): Low knowledge (< ", fmt_num(sc$mean, 2), ") and High knowledge (\u2265 ", fmt_num(sc$mean, 2), ").")))

# ------------------------------------------------------------------------------
# TABLE 4.4  ONE combined bivariate table (Sections A, B, C)
# ------------------------------------------------------------------------------
bt <- bv$table
lab_out <- bv$outcome_levels
fmt_es <- function(nm, v) paste0(nm, " = ", fmt_num(v, 2))
t44 <- list()
for (s in c("A", "B", "C")) {
  sec_lab <- bv$sections$section_label[bv$sections$section == s]
  sec_lab <- sub(" \\(V005 = 1\\)", "", sec_lab)
  nel <- bv$section_N$n_eligible[bv$section_N$section == s]
  t44[[length(t44) + 1]] <- tibble(row_type = "section", c1 = paste0(sec_lab, "; eligible N = ", nel),
                                   c2 = "", c3 = "", c4 = "", c5 = "", c6 = "", c7 = "")
  ids <- PREDICTORS %>% filter(biv_section == s) %>% pull(var_id)
  ids <- factor(ids, levels = c("V044", "V047", "V046", "V042", "V048", "V049", "V035", "V050", "V051", "V052", "V053", "V054"))
  for (vid in as.character(sort(ids))) {
    sp <- PREDICTORS[PREDICTORS$var_id == vid, ]
    r <- bt[bt$var_id == vid, ]
    first <- seq_len(nrow(r)) == 1
    t44[[length(t44) + 1]] <- tibble(row_type = "var",
      c1 = paste0(sp$label, ifelse(r$n_excluded[1] > 0, paste0(" [excluded: ", r$n_excluded[1], "]"), "")),
      c2 = "", c3 = "", c4 = "", c5 = "", c6 = "", c7 = "")
    t44[[length(t44) + 1]] <- tibble(row_type = "level", c1 = gsub(">=", "\u2265", r$level),
      c2 = fmt_npct(r$n_absent, r$total), c3 = fmt_npct(r$n_present, r$total), c4 = as.character(r$total),
      c5 = ifelse(first, r$statistic, "\u2014"),
      c6 = ifelse(first, fmt_es(r$effect_name, r$effect_size), "\u2014"),
      c7 = ifelse(first, fmt_p(r$p_value), "\u2014"))
  }
}
t44 <- bind_rows(t44)
fisher_vars <- bt %>% filter(method == "Fisher's exact test") %>% distinct(label) %>% pull(label)
tbl44 <- list(
  id = "t44",
  header = c("Characteristic / category", paste0(lab_out[1], ", n (%)"), paste0(lab_out[2], ", n (%)"), "Total, n",
             "Test statistic (df)", "Effect size", "P-value"),
  data = t44, widths = c(2.05, 0.90, 0.90, 0.45, 0.85, 0.80, 0.55),
  notes = c(
    "Outcome: current use of a modern contraceptive method. Percentages are row percentages calculated within each explanatory-variable category.",
    "Total is the number analysed in each category (non-missing outcome and predictor). One overall inferential test result is reported per predictor on its first category row.",
    paste0("Pearson chi-square without continuity correction was used when expected cell counts were adequate (no expected count < 1 and no more than 20% of cells < 5); otherwise Fisher's exact test. ",
           if (length(fisher_vars) == 0) "Assumptions were satisfied for all predictors." else paste0("Fisher's exact test was used for: ", paste(fisher_vars, collapse = ", "), ".")),
    "Effect size: \u03c6 for 2 \u00d7 2 tables and Cram\u00e9r's V for larger tables.",
    "Section B is restricted to currently married/cohabiting women (n = 299) per the questionnaire skip pattern; Sections A and C use the full sample (N = 419)."))

# ------------------------------------------------------------------------------
# TABLE 4.5  Crude and adjusted prevalence ratios (cPR / aPR)
# ------------------------------------------------------------------------------
cb <- mp$combined; dg <- mp$diagnostics
rng <- function(x) if (length(unique(x)) == 1) as.character(x[1]) else paste0(min(x), EN, max(x))
is_sig_pr <- function(p, lo, hi) {
  !is.na(p) & !is.na(lo) & !is.na(hi) & (p < ALPHA) & (round(lo, 2) > 1 | round(hi, 2) < 1)
}
fmt_p_sig <- function(p, lo, hi) {
  ifelse(is.na(p), "\u2014", paste0(fmt_p(p), ifelse(is_sig_pr(p, lo, hi), "*", "")))
}
t45 <- list()
for (pn in c("A", "B")) {
  g <- dg[dg$panel == pn, ]
  cp <- cb[cb$panel == pn, ]
  head_txt <- if (pn == "A")
    "PANEL A \u2014 FULL SAMPLE" else "PANEL B \u2014 WOMEN CURRENTLY MARRIED/COHABITING ONLY"
  t45[[length(t45) + 1]] <- tibble(row_type = "section",
    c1 = paste0(head_txt, " (n = ", g$n_model, "; events = ", g$events, ")"),
    c2 = "", c3 = "", c4 = "", c5 = "", c6 = "")
  ids <- intersect(TABLE45_ORDER, unique(cp$var_id))
  ids <- ids[ids %in% (if (pn == "A") PANEL_A_IDS else PANEL_B_IDS)]
  for (vid in ids) {
    sp <- PREDICTORS[PREDICTORS$var_id == vid, ]
    r <- cp[cp$var_id == vid, ]
    t45[[length(t45) + 1]] <- tibble(row_type = "level",
      c1 = c(sp$label, rep("", nrow(r) - 1)),
      c2 = gsub(">=", "\u2265", ifelse(r$is_ref, paste0(r$level, " (Ref.)"), r$level)),
      c3 = ifelse(r$is_ref, "1.00 (Ref.)", fmt_pr(r$cpr, r$cpr_lo, r$cpr_hi)),
      c4 = ifelse(r$is_ref, "\u2014", fmt_p_sig(r$cpr_p, r$cpr_lo, r$cpr_hi)),
      c5 = ifelse(r$is_ref, "1.00 (Ref.)", fmt_pr(r$apr, r$apr_lo, r$apr_hi)),
      c6 = ifelse(r$is_ref, "\u2014", fmt_p_sig(r$apr_p, r$apr_lo, r$apr_hi)))
  }
}
t45 <- bind_rows(t45)
tbl45 <- list(
  id = "t45",
  header = c("Predictor", "Category / contrast", "cPR (95% CI)", "P-value", "aPR (95% CI)", "P-value"),
  data = t45, widths = c(1.4, 2.0, 1.15, 0.6, 1.15, 0.6),
  notes = c(
    "cPR = crude prevalence ratio; aPR = adjusted prevalence ratio; CI = confidence interval; Ref. = reference category (PR = 1.00).",
    "* Statistically significant at P < 0.05 (95% CI does not include 1.00)."))

table_objects <- list(t41 = tbl41, t42 = tbl42, t43 = tbl43, t44 = tbl44, t45 = tbl45,
                      generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
save_rds(table_objects, RDS$tbl_obj)

# ------------------------------------------------------------------------------
# FIGURE DATA
# Figure 4.1 : participant flow (only stages supported by the records)
# ------------------------------------------------------------------------------
n_imp <- nrow(raw); n_cln <- nrow(cln); n_dup <- n_imp - n_cln
n_agebad <- sum(cln$QNA1_AGE < 15 | cln$QNA1_AGE > 49, na.rm = TRUE)
n_A <- dg$n_model[dg$panel == "A"]; n_B <- dg$n_model[dg$panel == "B"]
stopifnot(n_cln == N, n_agebad == 0)
node <- function(label, x, y, w, type) tibble(label = label, x = x, y = y, width = w, height = 0.85, type = type)
flow_nodes <- bind_rows(
  node(paste0("Records imported from KoboToolbox export\nn = ", n_imp), 0, 7.0, 5.8, "main"),
  node(paste0("Records after removing exact duplicate submissions\nn = ", n_cln), 0, 5.4, 5.8, "main"),
  node(paste0("Records meeting age eligibility (15\u201349 years)\nn = ", n_cln - n_agebad), 0, 3.8, 5.8, "main"),
  node(paste0("Final analytical sample (analysis-ready data)\nn = ", N), 0, 2.2, 5.8, "main"),
  node(paste0("Panel A models: full sample\nn = ", n_A), -2.7, 0.6, 4.3, "model"),
  node(paste0("Panel B models: married/cohabiting\nn = ", n_B), 2.7, 0.6, 4.3, "model"),
  node(paste0("Excluded: exact duplicate\nsubmissions\nn = ", n_dup), 6.5, 6.2, 4.0, "branch"),
  node(paste0("Excluded: outside age 15\u201349\nn = ", n_agebad), 6.5, 4.6, 4.0, "branch"))
flow_edges <- tribble(
  ~x, ~y, ~xend, ~yend,
  0, 6.575, 0, 5.825,
  0, 4.975, 0, 4.225,
  0, 3.375, 0, 2.625,
  0, 1.775, -2.7, 1.025,
  0, 1.775, 2.7, 1.025,
  0, 6.2, 4.5, 6.2,
  0, 4.6, 4.5, 4.6)

# Figure 4.2 : forest plot of adjusted PRs (verified aPR / 95% CI from Table 4.5)
short_forest_term <- function(var_id, label, level, ref_label) {
  lv <- gsub(">=", "\u2265", level)
  rf <- gsub(">=", "\u2265", ref_label)
  dplyr::case_when(
    var_id == "V044" ~ paste0("Age group: ", lv, " (vs. 15\u201324)"),
    var_id == "V047" ~ "Education: Secondary or higher (vs. Primary or less)",
    var_id == "V046" ~ "Parity: \u22653 children (vs. 0\u20132)",
    var_id == "V042" ~ "Knowledge: High (\u2265 mean) (vs. Low [< mean])",
    var_id == "V004" ~ paste0("Religion: ", lv, " (vs. Muslim)"),
    var_id == "V005" ~ "Marital status: Married/cohabiting (vs. Not in union)",
    var_id == "V007" & grepl("^Employed", lv) ~ "Occupation: Employed (vs. Self-employed)",
    var_id == "V007" ~ paste0("Occupation: ", lv, " (vs. Self-employed)"),
    var_id == "V048" ~ "Partner support: Supportive (vs. Not supportive/DK)",
    var_id == "V049" ~ "Decision making: Woman/Joint (vs. Husband/Other)",
    var_id == "V035" ~ paste0("Partner communication: ", lv, " (vs. Never)"),
    var_id == "V050" ~ "Infertility myth: Agree (vs. Disagree/DK)",
    var_id == "V051" ~ "Cancer myth: Agree (vs. Disagree/DK)",
    var_id == "V052" ~ "Infidelity myth: Agree (vs. Disagree/DK)",
    var_id == "V053" ~ "Cultural value of children: Agree (vs. Disagree/DK)",
    var_id == "V054" ~ "Religious acceptance: Allowed (vs. Not allowed/DK)",
    TRUE ~ paste0(label, ": ", lv, " (vs. ", rf, ")")
  )
}
forest <- mp$combined %>%
  filter(!is_ref, !is.na(apr)) %>%
  left_join(PREDICTORS %>% select(var_id, ref_label), by = "var_id") %>%
  mutate(
    term = short_forest_term(var_id, label, level, ref_label),
    estimate = apr, conf_low = apr_lo, conf_high = apr_hi, p = apr_p,
    sig = is_sig_pr(apr_p, apr_lo, apr_hi),
    plotted = TRUE,
    panel_label = ifelse(panel == "A",
                         paste0("Panel A: Full sample (n = ", n_A, ")"),
                         paste0("Panel B: Currently married or cohabiting women (n = ", n_B, ")"))
  ) %>%
  select(panel, var_id, term, estimate, conf_low, conf_high, p, sig, plotted, panel_label)
stopifnot(all(forest$estimate > 0), all(forest$conf_low > 0), all(forest$conf_high > 0))

figure_data <- list(
  flow = list(nodes = flow_nodes, edges = flow_edges,
              counts = list(imported = n_imp, duplicates_removed = n_dup, eligible = n_cln - n_agebad,
                            final = N, panelA = n_A, panelB = n_B, age_ineligible = n_agebad,
                            id_collisions_relabelled = 4, planned_sample = 427)),
  forest = forest,
  generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
save_rds(figure_data, RDS$fig_data)
cat("\nTables 4.1-4.5 and Figure data created.\n")
