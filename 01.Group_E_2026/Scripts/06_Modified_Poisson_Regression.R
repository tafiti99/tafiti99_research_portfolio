# ==============================================================================
# 06_Modified_Poisson_Regression.R
# TAFITI99 | GROUP E | Table 4.5 : unadjusted cPR (univariable models) and
#                                  adjusted aPR (multivariable models)
# INPUT : results/rds/data_04_analysis_ready.rds, results/rds/results_05_bivariate.rds
# OUTPUT: results/rds/models_06a_modified_poisson_unadjusted.rds
#         results/rds/models_06b_modified_poisson_adjusted.rds
#         results/rds/results_06c_modified_poisson_cPR_aPR.rds
# METHOD (Book Ch 15; Zou 2004; Barros & Hirakata 2003; Metadata analysis_plan
#         OBJ3-UV1/MV1 [Panel A, full sample] and OBJ3-UV2/MV2 [Panel B, union only]):
#   glm(outcome ~ predictors, family = poisson(link = "log")) with a robust
#   (sandwich, HC0) covariance estimator; PR = exp(beta); 95% CI = exp(beta +/- 1.96 SE_robust);
#   P-value from the robust Wald z-test. V005 is NOT entered in Panel B (constant).
# CANDIDATE RULE: bivariate P < 0.25 OR a priori / literature-supported predictor.
#   All 12 / 14 predictors listed in the analysis plan are a priori, so all are
#   retained; the P < 0.25 screen is still computed and stored for transparency.
# ==============================================================================
source(here::here("Scripts", "00_setup_config.R"), encoding = "UTF-8")
suppressPackageStartupMessages({ library(sandwich); library(lmtest); library(car) })
d  <- load_rds(RDS$ready)
bv <- load_rds(RDS$bivar)
section_banner("06 MODIFIED POISSON REGRESSION (cPR and aPR)")

# --- Reference categories (explicit, from PREDICTORS in 00_setup_config.R) --------------
prep_panel <- function(data, ids) {
  sp <- PREDICTORS[match(ids, PREDICTORS$var_id), ]
  for (i in seq_len(nrow(sp))) {
    v <- sp$var[i]
    if (v == "knowledge_level" && !KNOWLEDGE_KEY_VERIFIED) next
    f <- droplevels(data[[v]])
    stop_if(!sp$ref_label[i] %in% levels(f),
            paste0("Reference '", sp$ref_label[i], "' not a level of ", v))
    data[[v]] <- relevel(f, ref = sp$ref_label[i])
  }
  data
}
active_ids <- function(ids) if (KNOWLEDGE_KEY_VERIFIED) ids else setdiff(ids, "V042")

# --- Core fit: modified Poisson with robust variance ---------------------------------------
fit_mp <- function(data, vars) {
  f <- reformulate(vars, response = "outcome_use")
  fit <- tryCatch(glm(f, family = poisson(link = "log"), data = data),
                  error = function(e) NULL)
  if (is.null(fit)) {   # retry with sensible start values
    mm <- model.matrix(f, data)
    fit <- glm(f, family = poisson(link = "log"), data = data,
               start = c(log(mean(data$outcome_use)), rep(0, ncol(mm) - 1)))
  }
  fit
}
robust_table <- function(fit) {
  V <- vcovHC(fit, type = ROBUST_VCOV_TYPE)
  ct <- coeftest(fit, vcov. = V)
  tibble(coef_name = rownames(ct), beta = ct[, 1], se = ct[, 2], p = ct[, 4]) %>%
    mutate(pr = exp(beta), lo = exp(beta - qnorm(0.975) * se), hi = exp(beta + qnorm(0.975) * se))
}
overall_wald_p <- function(fit, var) {   # multi-df robust Wald test for one predictor
  V <- vcovHC(fit, type = ROBUST_VCOV_TYPE)
  idx <- grep(paste0("^", var), names(coef(fit)))
  b <- coef(fit)[idx]; Vi <- V[idx, idx, drop = FALSE]
  w <- as.numeric(t(b) %*% solve(Vi) %*% b)
  pchisq(w, length(idx), lower.tail = FALSE)
}

# map coefficients back to every category of a predictor (reference shown explicitly)
expand_terms <- function(fit_tab, data, var_id, n_model, model_type, panel) {
  sp <- PREDICTORS[PREDICTORS$var_id == var_id, ]
  lv_all <- levels(PREDICTORS_LEVELS[[sp$var]])
  map_dfr(lv_all, function(l) {
    is_ref <- l == sp$ref_label
    row <- fit_tab[fit_tab$coef_name == paste0(sp$var, l), ]
    tibble(panel = panel, var_id = var_id, var = sp$var, label = sp$label, level = l, is_ref = is_ref,
           pr = if (is_ref) 1 else row$pr, lo = if (is_ref) NA_real_ else row$lo,
           hi = if (is_ref) NA_real_ else row$hi, p = if (is_ref) NA_real_ else row$p,
           n_model = n_model, model_type = model_type)
  })
}
# display order of levels = Master Metadata order (religion_model from script 01)
PREDICTORS_LEVELS <- lapply(setNames(PREDICTORS$var, PREDICTORS$var), function(v) d[[v]])

panels <- list(
  A = list(label = "Panel A \u2014 full sample", data = d, ids = active_ids(PANEL_A_IDS)),
  B = list(label = "Panel B \u2014 currently married/cohabiting women (V005 = 1)",
           data = d %>% filter(in_union == 1), ids = active_ids(PANEL_B_IDS)))

unadj_models <- list(); adj_models <- list()
est_unadj <- list(); est_adj <- list()
selection <- list(); diagnostics <- list(); coef_full <- list()

for (pn in names(panels)) {
  pl <- panels[[pn]]
  dat <- prep_panel(pl$data, pl$ids)
  cat("\n", pl$label, "| N =", nrow(dat), "| events =", sum(dat$outcome_use), "\n")

  # ---- (a) UNADJUSTED: separate univariable models --------------------------------------
  unadj_models[[pn]] <- list()
  sel <- list()
  for (vid in pl$ids) {
    sp <- PREDICTORS[PREDICTORS$var_id == vid, ]
    dd <- dat[!is.na(dat[[sp$var]]) & !is.na(dat$outcome_use), ]
    fit <- fit_mp(dd, sp$var)
    unadj_models[[pn]][[vid]] <- fit
    tab <- robust_table(fit)
    est_unadj[[paste(pn, vid)]] <- expand_terms(tab, dd, vid, nrow(dd), "cPR", pn)
    p_wald <- overall_wald_p(fit, sp$var)
    # Match Table 4.4 chi-square/Fisher P-value only when computed on the SAME population
    # (Sections A and C = full sample [Panel A]; Section B = married/cohabiting [Panel B]).
    biv_sec <- if (pn == "A") c("A", "C") else "B"
    biv_sub <- bv$p_by_variable %>% filter(section %in% biv_sec, var_id == vid)
    p_biv <- if (nrow(biv_sub) == 1) biv_sub$p_value else NA_real_
    sel[[vid]] <- tibble(panel = pn, var_id = vid, label = sp$label,
                         p_bivariate = ifelse(is.na(p_biv), p_wald, p_biv),
                         p_source = ifelse(is.na(p_biv), "univariable robust Wald (panel-specific)", "Table 4.4 chi-square/Fisher (panel-specific)"),
                         p_univariable_wald = p_wald,
                         passes_p025 = ifelse(is.na(p_biv), p_wald, p_biv) < CANDIDATE_P,
                         a_priori = sp$a_priori)
  }
  sel <- bind_rows(sel) %>% mutate(retained = passes_p025 | a_priori)
  selection[[pn]] <- sel
  adj_ids <- sel$var_id[sel$retained]
  adj_vars <- PREDICTORS$var[match(adj_ids, PREDICTORS$var_id)]

  # ---- (b) ADJUSTED: one multivariable model ---------------------------------------------
  dd <- dat[complete.cases(dat[, c("outcome_use", adj_vars)]), ]
  fit <- fit_mp(dd, adj_vars)
  adj_models[[pn]] <- fit
  tab <- robust_table(fit)
  coef_full[[pn]] <- tab
  for (vid in adj_ids) est_adj[[paste(pn, vid)]] <- expand_terms(tab, dd, vid, nrow(dd), "aPR", pn)

  gv <- tryCatch({ g <- car::vif(fit); if (is.matrix(g)) g[, 3]^2 else g }, error = function(e) NA)
  n_par <- length(coef(fit)) - 1
  diagnostics[[pn]] <- tibble(
    panel = pn, population = pl$label, n_model = nrow(dd), n_excluded = nrow(pl$data) - nrow(dd),
    events = sum(dd$outcome_use), n_parameters = n_par, events_per_parameter = sum(dd$outcome_use) / n_par,
    converged = fit$converged, any_nonfinite_estimate = any(!is.finite(tab$pr)),
    max_robust_se = max(tab$se[-1]), max_gvif_adj = suppressWarnings(max(unlist(gv), na.rm = TRUE)),
    vcov_type = ROBUST_VCOV_TYPE, covariates = paste(adj_vars, collapse = "; "))
  print(as.data.frame(diagnostics[[pn]][, c("n_model", "events", "n_parameters", "events_per_parameter",
                                            "converged", "max_robust_se", "max_gvif_adj")]))
  stop_if(!fit$converged, paste("Adjusted model did not converge for panel", pn))
}

unadj <- bind_rows(est_unadj); adj <- bind_rows(est_adj)
combined <- unadj %>%
  select(panel, var_id, var, label, level, is_ref, cpr = pr, cpr_lo = lo, cpr_hi = hi, cpr_p = p, n_unadj = n_model) %>%
  left_join(adj %>% select(panel, var_id, level, apr = pr, apr_lo = lo, apr_hi = hi, apr_p = p, n_adj = n_model),
            by = c("panel", "var_id", "level")) %>%
  mutate(var_id = factor(var_id, levels = TABLE45_ORDER)) %>%
  arrange(panel, var_id) %>% mutate(var_id = as.character(var_id))

# The unadjusted/adjusted display text is built in script 07; keep numeric values here.
on_hold <- if (KNOWLEDGE_KEY_VERIFIED) character() else "V042"

ref_table <- PREDICTORS %>% filter(var_id %in% TABLE45_ORDER) %>%
  select(var_id, label, ref_label, ref_rule)

cat("\nSelection (all predictors a priori -> retained):\n")
print(as.data.frame(bind_rows(selection) %>% select(panel, var_id, label, p_bivariate, passes_p025, retained)), digits = 3)
cat("\nAdjusted estimates:\n")
print(as.data.frame(combined %>% filter(!is_ref) %>%
        transmute(panel, var_id, level, cPR = round(cpr, 2), cp = round(cpr_p, 3), aPR = round(apr, 2), aLo = round(apr_lo, 2), aHi = round(apr_hi, 2), ap = round(apr_p, 3))), digits = 3)

save_rds(list(models = unadj_models, estimates = unadj, vcov_type = ROBUST_VCOV_TYPE), RDS$mp_unadj)
save_rds(list(models = adj_models, estimates = adj, coefficients_full = coef_full, vcov_type = ROBUST_VCOV_TYPE), RDS$mp_adj)
save_rds(list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  method = "Modified Poisson regression (log link, robust HC0 sandwich SE); PR = exp(beta); Wald 95% CI",
  combined = combined, selection = bind_rows(selection), diagnostics = bind_rows(diagnostics),
  reference_table = ref_table, on_hold = on_hold,
  religion_note = "'No religion' (n=2) and 'Other' (n=1) merged for regression only (sparse; 'Other' has zero events)."
), RDS$mp_all)
