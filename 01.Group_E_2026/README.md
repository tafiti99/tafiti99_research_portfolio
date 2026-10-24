# Modern Contraceptive Use in Mkuranga District: Knowledge & Socio-Cultural Predictors

> **Project Identifier**: `01.Group_E_2026`  
> **Study Title**: *Assessment of Knowledge and Socio-Cultural Factors Affecting Modern Contraceptive Use among Women of Reproductive Age at Mkuranga District, Pwani Region*  
> **Analytical Framework**: Reproducible Script-to-RDS Pipeline & Quarto Dynamic Reporting in R  

---

## 📌 1. Project Overview & Background

Modern contraceptive uptake remains a critical indicator of maternal and reproductive health in sub-Saharan Africa. This cross-sectional analytical study evaluates the influence of comprehensive contraceptive knowledge, sociodemographic parameters, and deeply rooted socio-cultural norms on current modern contraceptive adoption among women of reproductive age (15–49 years) residing in Mkuranga District, Pwani Region, Tanzania.

### Specific Objectives
1. **Estimate Prevalence**: Determine the current prevalence and method mix of modern contraceptive use.
2. **Assess Knowledge**: Evaluate domain-specific awareness of contraceptive methods, perceived benefits, and potential side-effects.
3. **Identify Determinants**: Quantify independent associations between socio-demographic, partner-related, and socio-cultural belief factors and modern contraceptive practice using multivariable regression.

---

## 🔬 2. Methodological & Statistical Rigor

### Why Modified Poisson Regression?
In cross-sectional epidemiological studies where the outcome prevalence is common (> 10%), conventional Logistic Regression yields **Odds Ratios (OR)** that substantially inflate the true relative effect (Prevalence Ratio). 

To prevent misinterpretation and effect-size inflation, this study employs **Modified Poisson Regression with Robust (Sandwich) Standard Errors** (Zou, 2004; *SPSS & R for Health Research*, Ch. 15):
- **Model Specification**: Generalized Linear Model (GLM) with a Poisson family distribution and a log link function:
  $$\log(P(Y=1|X)) = \beta_0 + \sum \beta_i X_i$$
- **Variance Estimation**: The Huber-White sandwich estimator (`sandwich::vcovHC(..., type = "HC0")`) corrects for Poisson misspecification / overdispersion in binary data, producing valid 95% Confidence Intervals and $p$-values.
- **Reported Estimates**: Exponentiated coefficients represent **Crude Prevalence Ratios (cPR)** for univariable analyses and **Adjusted Prevalence Ratios (aPR)** for multivariable models.

### Two-Panel Analytical Stratification
- **Panel A (Full Sample)**: Evaluates universal determinants across all eligible women (e.g., age, maternal education, religion, occupational status, parity, socio-cultural myths).
- **Panel B (Married / In-Union Subsample)**: Restricts analysis to currently married or cohabiting women to rigorously examine spousal dynamics, male partner approval, and shared reproductive decision-making without introducing structural missingness.

---

## 📂 3. Repository Architecture

```text
01.Group_E_2026/
├── 01.Group_E_2026.Rproj                  # RStudio Project Root
├── run_all.R                              # Master replication runner (one-click execution)
├── session_info.txt                       # R session & exact package version lock
├── data/
│   ├── class_work_2_raw.xlsx              # Raw survey dataset from field collection
│   ├── class work 2 - SPSS labels.sps     # SPSS variable & value labels syntax
│   └── groupE_cleaned.csv                 # Cleaned dataset (intermediate)
├── figures/
│   └── diagnostics/                       # Exploratory & diagnostic visual plots
│       ├── diag_age_histogram.png
│       ├── diag_missingness.png
│       └── diag_parity_bar.png
├── reports/
│   ├── Chapter4_Results.qmd               # Quarto dynamic results chapter (presentation layer)
│   ├── Chapter4_Results.docx              # Rendered publication-ready Word chapter
│   ├── reference.docx                     # Typography & style reference template
│   └── poster/                            # Social media & presentation poster assets
│       ├── poster.png
│       ├── poster.pdf
│       ├── caption_linkedin.txt
│       └── caption_instagram.txt
├── resources/
│   ├── GroupE_Chapter4_Blueprint_v5.xlsx  # Complete analytical blueprint & variable mapping
│   ├── Research proposal-final 2.docx     # Approved study protocol & proposal
│   └── TAFITI99_Master_Research_Metadata_v3.xlsx # Master metadata dictionary & coding key
├── results/
│   ├── rds/                               # Checkpoint storage for pipeline verification
│   │   ├── audit_02_exploration.rds
│   │   ├── data_01_imported_raw.rds
│   │   ├── data_03_cleaned_checked.rds
│   │   ├── data_04_analysis_ready.rds
│   │   ├── models_06a_modified_poisson_unadjusted.rds
│   │   ├── models_06b_modified_poisson_adjusted.rds
│   │   ├── results_04_descriptive.rds
│   │   ├── results_05_bivariate.rds
│   │   ├── results_06c_modified_poisson_cPR_aPR.rds
│   │   ├── results_07_figure_data.rds
│   │   └── results_07_table_objects.rds
│   └── tables/
│       └── cleaning_log.csv               # Data cleaning audit trail
└── Scripts/
    ├── 00_setup_config.R                  # Shared configuration, paths & metadata loader
    ├── 01_Import_Explore_Clean_Transform.R# Ingestion, validation, cleaning, and recoding
    ├── 04_Descriptive_Analysis.R          # Descriptive statistics (Table 4.1, 4.2, 4.3)
    ├── 05_Bivariate_Analysis.R            # Cross-tabulations & association tests (Table 4.4)
    ├── 06_Modified_Poisson_Regression.R   # Modified Poisson modeling (Table 4.5 cPR/aPR)
    ├── 07_Tables_Figures.R                # Flextable objects & ggplot2 forest/flow charts
    └── helpers_report.R                   # Quarto rendering helper functions
```

---

## ⚙️ 4. Analytical Pipeline & Workflow

The analysis follows a strictly verified **Checkpoint Architecture (Blueprint v5)**. Each phase reads from the previous serialized checkpoint (`.rds`) and saves its verified state, ensuring end-to-end auditability and separation between data transformation, modeling, and presentation:

1. **`00_setup_config.R`**:
   Sets CRAN packages (`dplyr`, `tidyr`, `ggplot2`, `sandwich`, `lmtest`, `here`), establishes path definitions relative to the project root, sets significance thresholds ($\alpha = 0.05$), and defines the predictor specification with reference categories.

2. **`01_Import_Explore_Clean_Transform.R`**:
   Imports raw data, checks duplicate questionnaire submissions, re-labels non-unique IDs, validates range boundaries, recodes variables according to `TAFITI99_Master_Research_Metadata_v3.xlsx`, and logs all transformation steps into `cleaning_log.csv`.

3. **`04_Descriptive_Analysis.R`**:
   Calculates descriptive frequencies, proportions, and continuous metrics (mean $\pm$ SD, median and IQR) for sociodemographic variables, modern contraceptive uptake prevalence with 95% Wilson/Score CIs, and item-level knowledge indicators.

4. **`05_Bivariate_Analysis.R`**:
   Performs hypothesis testing across candidate predictors using Pearson’s Chi-Square test (or Fisher’s Exact test where cell counts $< 5$) and calculates unadjusted effect sizes.

5. **`06_Modified_Poisson_Regression.R`**:
   Fits univariable and multivariable Generalized Linear Models (`family = poisson(link = "log")`). Computes robust standard errors via `sandwich::vcovHC(model, type = "HC0")` and Wald $p$-values via `lmtest::coeftest()`, exporting clean `cPR` and `aPR` tables with 95% CIs.

6. **`07_Tables_Figures.R`**:
   Builds styled APA-compliant tables via `flextable` and renders publication-ready vector figures:
   - *Figure 4.1*: Participant flow diagram from raw export to final analytical sample.
   - *Figure 4.2*: Two-panel forest plot displaying multivariable Adjusted Prevalence Ratios on a log scale.

7. **`Chapter4_Results.qmd`**:
   Quarto report document that consumes the serialized RDS checkpoints and compiles directly into `Chapter4_Results.docx` with automated inline statistical narratives.

---

## 🚀 5. How to Reproduce

### Prerequisites
Install [R (>= 4.3.0)](https://cran.r-project.org/) and optionally [RStudio](https://posit.co/download/rstudio-desktop/) or VS Code with the R extension.

Ensure the required packages are installed:
```r
install.packages(c(
  "dplyr", "tidyr", "tibble", "purrr", "stringr", 
  "readr", "readxl", "ggplot2", "here", "sandwich", 
  "lmtest", "flextable", "officer", "quarto"
))
```

### Running the Complete Pipeline

#### Option A: One-Command Automated Replication (Recommended)
Open `01.Group_E_2026.Rproj` in RStudio or set working directory to this folder, then run:

```r
source("run_all.R")
```

Or execute directly from your terminal / command prompt:
```bash
Rscript run_all.R
```

This single command executes all data cleaning, validation, bivariate testing, Modified Poisson modeling, table/figure formatting, and renders `reports/Chapter4_Results.docx` in ~38 seconds.

#### Option B: Step-by-Step Execution
If you prefer running individual analytical phases manually:

```r
# Execute pipeline scripts sequentially
source("Scripts/00_setup_config.R")
source("Scripts/01_Import_Explore_Clean_Transform.R")
source("Scripts/04_Descriptive_Analysis.R")
source("Scripts/05_Bivariate_Analysis.R")
source("Scripts/06_Modified_Poisson_Regression.R")
source("Scripts/07_Tables_Figures.R")

# Render the Quarto report to DOCX
quarto::quarto_render("reports/Chapter4_Results.qmd")
```

---

## 📊 6. Summary of Key Deliverables

- **Chapter 4 Document**: [`reports/Chapter4_Results.docx`](./reports/Chapter4_Results.docx)
- **Table 4.1**: Sociodemographic characteristics of study participants.
- **Table 4.2**: Modern contraceptive prevalence and method breakdown.
- **Table 4.3**: Itemized knowledge of methods, advantages, and side effects.
- **Table 4.4**: Bivariate associations with modern contraceptive use.
- **Table 4.5**: Multivariable Modified Poisson Regression (cPR & aPR with 95% CIs).
- **Figure 4.1 & 4.2**: Participant flowchart and multivariable forest plot.
