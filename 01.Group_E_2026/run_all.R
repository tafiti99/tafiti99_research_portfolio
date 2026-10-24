# ==============================================================================
# run_all.R
# TAFITI99 | GROUP E (01.Group_E_2026)
# Master Automated Replication Runner
# ------------------------------------------------------------------------------
# Executes the entire analytical pipeline sequentially:
#   1. Setup & configuration (00_setup_config.R)
#   2. Data import, cleaning, transformation & validation (01_Import_...)
#   3. Descriptive statistics & composite knowledge scoring (04_Descriptive_...)
#   4. Bivariate hypothesis testing & effect sizes (05_Bivariate_...)
#   5. Modified Poisson regression with robust sandwich variance (06_Modified_Poisson_...)
#   6. Publication tables (4.1–4.5) & figures (4.1–4.2) formatting (07_Tables_Figures.R)
#   7. Automated Quarto report rendering (Chapter4_Results.qmd -> .docx)
# ==============================================================================

t_start <- Sys.time()
cat("\n", strrep("=", 78), "\n", sep = "")
cat("TAFITI99 RESEARCH PIPELINE REPLICATION: 01.Group_E_2026\n")
cat("Started at:", format(t_start, "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 78), "\n\n", sep = "")

# 1. Ensure working directory is set to 01.Group_E_2026
if (file.exists("01.Group_E_2026") && dir.exists("01.Group_E_2026")) {
  setwd("01.Group_E_2026")
}

# 2. Check required dependencies
req_packages <- c(
  "dplyr", "tidyr", "tibble", "purrr", "stringr", "readr", "readxl",
  "ggplot2", "here", "sandwich", "lmtest", "flextable", "officer"
)

missing_pkgs <- req_packages[!vapply(req_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_pkgs) > 0) {
  stop(sprintf("Missing required R packages: %s\nPlease install them using install.packages() before proceeding.",
               paste(missing_pkgs, collapse = ", ")), call. = FALSE)
}

# 3. Pipeline scripts in sequential order
scripts <- c(
  file.path("Scripts", "00_setup_config.R"),
  file.path("Scripts", "01_Import_Explore_Clean_Transform.R"),
  file.path("Scripts", "04_Descriptive_Analysis.R"),
  file.path("Scripts", "05_Bivariate_Analysis.R"),
  file.path("Scripts", "06_Modified_Poisson_Regression.R"),
  file.path("Scripts", "07_Tables_Figures.R")
)

for (s in scripts) {
  s_path <- here::here(s)
  if (!file.exists(s_path)) {
    stop(paste("Pipeline script not found:", s_path), call. = FALSE)
  }
  cat("--> Sourcing:", s, "...\n")
  source(s_path, encoding = "UTF-8")
}

# 4. Render Quarto publication report
cat("\n--> Rendering Quarto report (reports/Chapter4_Results.qmd)...\n")
qmd_path <- here::here("reports", "Chapter4_Results.qmd")
if (file.exists(qmd_path)) {
  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_render(qmd_path, quiet = FALSE)
  } else {
    # Fallback to system command if quarto R package is not installed
    system2("quarto", args = c("render", shQuote(qmd_path)))
  }
  cat("  [success] Report rendered: reports/Chapter4_Results.docx\n")
} else {
  warning("Report file reports/Chapter4_Results.qmd not found. Skipping Quarto rendering.")
}

t_end <- Sys.time()
cat("\n", strrep("=", 78), "\n", sep = "")
cat("REPLICATION COMPLETED SUCCESSFULLY in", round(difftime(t_end, t_start, units = "secs"), 1), "seconds.\n")
cat("Finished at:", format(t_end, "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 78), "\n\n", sep = "")
