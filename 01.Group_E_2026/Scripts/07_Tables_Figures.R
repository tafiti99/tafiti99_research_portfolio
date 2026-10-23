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
HOLD <- !KNOWLEDGE_KEY_VERIFIED
HOLD_TXT <- "On hold\u2020"
HOLD_NOTE <- paste0("\u2020 On hold: the composite knowledge score (V041) and knowledge level (V042) are not derived ",
                    "because the item-level correct-response key and the missing-item rule have not yet been verified ",
                    "against the source questionnaire/scoring documentation (Master Metadata, critical confirmation 2). ",
                    "Dependent rows will be completed once the key is confirmed.")

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
    paste0("The summary statistic for each continuous variable was selected after distributional assessment (mean (SD) if |skewness| < ", SKEW_RULE, "; otherwise median (Q1\u2013Q3)); age skewness = ",
           fmt_num(cont$skewness[1], 2), ", living-children skewness = ", fmt_num(cont$skewness[2], 2), "."),
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
    "Outcome: V031 current use of any modern contraceptive method. Prevalence = number currently using / number with available outcome data \u00d7 100.",
    paste0("The 95% confidence interval was calculated with the ", PREV_CI_METHOD, " (exact Clopper\u2013Pearson interval: ",
           fmt_num(pv$exact_ci_lo, 1), EN, fmt_num(pv$exact_ci_hi, 1), "%)."),
    paste0("Records with missing outcome data excluded from the denominator: ", pv$n_excluded_missing_outcome, " of ", pv$n_total_records, ".")))

# ------------------------------------------------------------------------------
# TABLE 4.3  Knowledge items (+ composite rows on hold)
# ------------------------------------------------------------------------------
ki <- ds$knowledge_items
t43 <- ki %>% transmute(
  domain, item,
  yes = fmt_npct(n_yes, n_valid), no = fmt_npct(n_no, n_valid),
  variable = variable_id, valid = paste0(n_valid, "; ", n_missing))
kc <- ds$knowledge_composite
if (HOLD) {
  t43 <- bind_rows(t43, tibble(
    domain = c("Composite knowledge score", "Knowledge level", "Knowledge level"),
    item = c("Knowledge score (0\u201318)", "Low knowledge (< mean)", "High knowledge (\u2265 mean)"),
    yes = HOLD_TXT, no = "\u2014", variable = c("V041", "V042", "V042"), valid = "\u2014"))
} else {
  lv <- kc$level
  sc <- kc$score
  sc_disp <- paste0(fmt_num(sc$mean, 2), " (", fmt_num(sc$sd, 2), ") [median ",
                    fmt_num(sc$median, 0), ", Q1\u2013Q3 ", fmt_num(sc$q1, 0), EN, fmt_num(sc$q3, 0),
                    "; range ", fmt_num(sc$min, 0), EN, fmt_num(sc$max, 0), "]")
  t43 <- bind_rows(t43, tibble(
    domain = c("Composite knowledge score", "Knowledge level", "Knowledge level"),
    item = c(paste0("Knowledge score (0\u201318), ", sc$statistic_used),
             "Low knowledge (< mean)", "High knowledge (\u2265 mean)"),
    yes = c(sc_disp, fmt_npct(lv$n[1], lv$n_valid[1]), fmt_npct(lv$n[2], lv$n_valid[2])),
    no = "\u2014", variable = c("V041", "V042", "V042"),
    valid = paste0(sc$n_valid, "; ", sc$n_missing)))
}
tbl43 <- list(
  id = "t43", header = c("Knowledge domain", "Knowledge item / summary", "Yes, n (%) / summary", "No, n (%)", "Variable", "Valid n; missing n"),
  data = t43, widths = c(1.35, 1.9, 1.05, 0.9, 0.6, 0.7), group_col = "domain",
  notes = c(
    "Each item uses its own available-response denominator. The 18 items are V010\u2013V015, V016\u2013V023 and V025\u2013V028; free-text and 'other' responses (V024, V029, V030) are not part of the specified score.",
    "Yes/No are the questionnaire response categories (Yes = aware / knows the advantage / knows the side effect); they are not labelled correct/incorrect here.",
    if (HOLD) HOLD_NOTE else paste0("Knowledge score (V041) = sum of 'Yes' (code 1) responses across the 18 items (range 0\u201318; 0 missing items). Knowledge level (V042) dichotomises participants at the sample mean (", fmt_num(kc$score$mean, 2), "): Low knowledge (< ", fmt_num(kc$score$mean, 2), ") and High knowledge (\u2265 ", fmt_num(kc$score$mean, 2), "), matching Scripts/Group E.R.")))

# ------------------------------------------------------------------------------
# TABLE 4.4  ONE combined bivariate table (Sections A, B, C)
# ------------------------------------------------------------------------------
bt <- bv$table
lab_out <- bv$outcome_levels
fmt_es <- function(nm, v) paste0(nm, " = ", fmt_num(v, 2))
t44 <- list()
for (s in c("A", "B", "C")) {
  sec_lab <- bv$sections$section_label[bv$sections$section == s]
  nel <- bv$section_N$n_eligible[bv$section_N$section == s]
  t44[[length(t44) + 1]] <- tibble(row_type = "section", c1 = paste0(sec_lab, "; eligible N = ", nel),
                                   c2 = "", c3 = "", c4 = "", c5 = "", c6 = "", c7 = "")
  ids <- PREDICTORS %>% filter(biv_section == s) %>% pull(var_id)
  ids <- factor(ids, levels = c("V044", "V047", "V046", "V042", "V048", "V049", "V035", "V050", "V051", "V052", "V053", "V054"))
  for (vid in as.character(sort(ids))) {
    sp <- PREDICTORS[PREDICTORS$var_id == vid, ]
    if (vid == "V042" && HOLD) {
      lv <- gsub(">=", "\u2265", levels(ar$knowledge_level))
      t44[[length(t44) + 1]] <- tibble(row_type = "var", c1 = paste0(sp$label, " (V042) \u2014 on hold\u2020"),
                                       c2 = "", c3 = "", c4 = "", c5 = "", c6 = "", c7 = "")
      t44[[length(t44) + 1]] <- tibble(row_type = "level", c1 = lv,
                                       c2 = HOLD_TXT, c3 = HOLD_TXT, c4 = "\u2014",
                                       c5 = c(HOLD_TXT, "\u2014"), c6 = c(HOLD_TXT, "\u2014"), c7 = c(HOLD_TXT, "\u2014"))
      next
    }
    r <- bt[bt$var_id == vid, ]
    first <- seq_len(nrow(r)) == 1
    t44[[length(t44) + 1]] <- tibble(row_type = "var",
      c1 = paste0(sp$label, " (", vid, ")", ifelse(r$n_excluded[1] > 0, paste0(" [excluded: ", r$n_excluded[1], "]"), "")),
      c2 = "", c3 = "", c4 = "", c5 = "", c6 = "", c7 = "")
    t44[[length(t44) + 1]] <- tibble(row_type = "level", c1 = gsub(">=", "\u2265", r$level),
      c2 = fmt_npct(r$n_absent, r$total), c3 = fmt_npct(r$n_present, r$total), c4 = as.character(r$total),
      c5 = ifelse(first, r$statistic, "\u2014"),
      c6 = ifelse(first, fmt_es(r$effect_name, r$effect_size), "\u2014"),
      c7 = ifelse(first, fmt_p(r$p_value), "\u2014"))
  }
}
t44 <- bind_rows(t44)
methods_used <- bt %>% distinct(var_id, method) %>% count(method)
fisher_vars <- bt %>% filter(method == "Fisher's exact test") %>% distinct(label) %>% pull(label)
tbl44 <- list(
  id = "t44",
  header = c("Characteristic / category", paste0(lab_out[1], ", n (%)"), paste0(lab_out[2], ", n (%)"), "Total, n",
             "Test statistic (df)", "Effect size", "P-value"),
  data = t44, widths = c(2.05, 0.90, 0.90, 0.45, 0.85, 0.80, 0.55),
  notes = c(
    "Outcome: V031 current use of a modern contraceptive method. Percentages are row percentages, calculated within each explanatory-variable category.",
    "Total is the number analysed in each category (non-missing outcome and predictor); the section eligible N is not a substitute for the variable-specific denominator. One overall result is reported per predictor, on its first category row.",
    paste0("Pearson chi-square without continuity correction was used when assumptions were adequate (no expected count < 1 and no more than 20% of cells with expected count < 5); otherwise Fisher's exact test. ",
           if (length(fisher_vars) == 0) "Fisher's exact test was not required for any predictor." else paste0("Fisher's exact test was used for: ", paste(fisher_vars, collapse = ", "), ".")),
    "Effect size: \u03c6 for 2 \u00d7 2 tables and Cram\u00e9r's V for larger tables (association strength, not a prevalence ratio).",
    "Section B is restricted to currently married/cohabiting women (V005 = 1) because of the questionnaire skip pattern; Sections A and C use the full sample.",
    if (HOLD) HOLD_NOTE))

# ------------------------------------------------------------------------------
# TABLE 4.5  Crude and adjusted prevalence ratios (cPR / aPR)
# ------------------------------------------------------------------------------
cb <- mp$combined; dg <- mp$diagnostics
rng <- function(x) if (length(unique(x)) == 1) as.character(x[1]) else paste0(min(x), EN, max(x))
t45 <- list()
for (pn in c("A", "B")) {
  g <- dg[dg$panel == pn, ]
  cp <- cb[cb$panel == pn, ]
  head_txt <- if (pn == "A")
    "PANEL A \u2014 FULL SAMPLE" else "PANEL B \u2014 WOMEN CURRENTLY MARRIED/COHABITING ONLY (V005 = 1)"
  t45[[length(t45) + 1]] <- tibble(row_type = "section",
    c1 = paste0(head_txt, "; unadjusted models n = ", rng(cp$n_unadj), "; adjusted model n = ", g$n_model,
                " (events = ", g$events, ")"), c2 = "", c3 = "", c4 = "", c5 = "", c6 = "")
  ids <- intersect(TABLE45_ORDER, c(unique(cp$var_id), if (HOLD) "V042"))
  ids <- ids[ids %in% (if (pn == "A") PANEL_A_IDS else PANEL_B_IDS)]
  for (vid in ids) {
    sp <- PREDICTORS[PREDICTORS$var_id == vid, ]
    if (vid == "V042" && HOLD) {
      lv <- gsub(">=", "\u2265", levels(ar$knowledge_level))
      t45[[length(t45) + 1]] <- tibble(row_type = "level", c1 = c("Knowledge level \u2020", ""), c2 = lv,
        c3 = HOLD_TXT, c4 = HOLD_TXT, c5 = HOLD_TXT, c6 = HOLD_TXT)
      next
    }
    r <- cp[cp$var_id == vid, ]
    lbl <- ifelse(vid == "V004", "Religion \u2021", sp$label)
    t45[[length(t45) + 1]] <- tibble(row_type = "level",
      c1 = c(lbl, rep("", nrow(r) - 1)),
      c2 = gsub(">=", "\u2265", ifelse(r$is_ref, paste0(r$level, " (Ref.)"), r$level)),
      c3 = ifelse(r$is_ref, "1.00 (Ref.)", fmt_pr(r$cpr, r$cpr_lo, r$cpr_hi)),
      c4 = ifelse(r$is_ref, "\u2014", fmt_p(r$cpr_p)),
      c5 = ifelse(r$is_ref, "1.00 (Ref.)", fmt_pr(r$apr, r$apr_lo, r$apr_hi)),
      c6 = ifelse(r$is_ref, "\u2014", fmt_p(r$apr_p)))
  }
}
t45 <- bind_rows(t45)
adj_cov <- function(pn) {
  ids <- mp$selection$var_id[mp$selection$panel == pn & mp$selection$retained]
  paste(PREDICTORS$label[match(ids, PREDICTORS$var_id)], collapse = ", ")
}
tbl45 <- list(
  id = "t45",
  header = c("Predictor", "Category / contrast", "cPR (95% CI)", "P-value", "aPR (95% CI)", "P-value"),
  data = t45, widths = c(1.4, 2.0, 1.15, 0.6, 1.15, 0.6),
  notes = c(
    "Outcome: V031 current use of a modern contraceptive method. Modified Poisson regression (log link) with robust (HC0 sandwich) standard errors. cPR = crude prevalence ratio from separate univariable models; aPR = adjusted prevalence ratio from the prespecified multivariable model; CI = confidence interval; Ref. = reference category (PR = 1.00).",
    "Candidate selection followed Master Metadata: bivariate P < 0.25 OR a priori/literature-supported predictor retained irrespective of P. All listed predictors are a priori and were retained; alternative representations of the same construct were not entered together.",
    paste0("Panel A adjusted for: ", adj_cov("A"), ". Panel B adjusted for: ", adj_cov("B"), ". Marital status is not estimable in Panel B (constant after restriction)."),
    paste0("Reference categories are not specified in Master Metadata; they were fixed a priori (ordinal: lowest category; binary: unexposed state; multi-category nominal: most prevalent category) and are marked (Ref.)."),
    paste0("\u2021 'No religion' (n = 2) and 'Other' (n = 1) were merged for the regression only because each is too sparse for a stable estimate ('Other' has no current user). Estimates for this merged category are very imprecise."),
    paste0("Events per estimated parameter: Panel A = ", fmt_num(dg$events_per_parameter[dg$panel == "A"], 1),
           "; Panel B = ", fmt_num(dg$events_per_parameter[dg$panel == "B"], 1), ". Estimates should be interpreted with caution. All models converged; largest degrees-of-freedom-adjusted generalised VIF (GVIF^(1/df)) = ",
           fmt_num(max(dg$max_gvif_adj), 2), "."),
    if (HOLD) HOLD_NOTE))

table_objects <- list(t41 = tbl41, t42 = tbl42, t43 = tbl43, t44 = tbl44, t45 = tbl45,
                      knowledge_hold = HOLD, generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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
forest <- mp$combined %>%
  filter(!is_ref, !is.na(apr)) %>%
  left_join(PREDICTORS %>% select(var_id, ref_label), by = "var_id") %>%
  transmute(panel, var_id, term = gsub(">=", "\u2265", paste0(label, ": ", level, " vs ", ref_label)),
            estimate = apr, conf_low = apr_lo, conf_high = apr_hi, p = apr_p,
            plotted = !(var_id == "V004" & level == "No religion / Other"),
            panel_label = ifelse(panel == "A", "Panel A: full sample", "Panel B: married/cohabiting women"))
stopifnot(all(forest$estimate > 0), all(forest$conf_low > 0), all(forest$conf_high > 0))

figure_data <- list(
  flow = list(nodes = flow_nodes, edges = flow_edges,
              counts = list(imported = n_imp, duplicates_removed = n_dup, eligible = n_cln - n_agebad,
                            final = N, panelA = n_A, panelB = n_B, age_ineligible = n_agebad,
                            id_collisions_relabelled = 4, planned_sample = 427)),
  forest = forest,
  forest_note = "Merged 'No religion / Other' category not plotted (CI too wide for a log axis to be informative); see Table 4.5.",
  generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
save_rds(figure_data, RDS$fig_data)
cat("\nTables 4.1-4.5 and Figure data created.\n")
