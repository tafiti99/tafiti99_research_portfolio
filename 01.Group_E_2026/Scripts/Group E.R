# ---------------------------------------------------------
# REPRODUCIBLE KOBO TO R WORKFLOW USING SPSS SYNTAX
# ---------------------------------------------------------
library(dplyr)
library(haven)
library(readxl)
library(stringr)
library(fs)
fs::dir_tree()
# 1. Load the raw data USING YOUR ORIGINAL IMPORT LOGIC
class_work_2_raw <- read_excel("data/class_work_2_raw.xlsx", 
                               col_types = c("skip", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "text", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "text", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "skip", "skip", "skip", 
                                             "skip", "skip", "skip", "skip", "skip", 
                                             "skip", "skip", "skip"))
# 2. Read the SPSS syntax file (.sps) as text
sps_file <- "data/class work 2 - SPSS labels.sps" 
sps_lines <- readLines(sps_file, warn = FALSE)

# 3. Parse the VALUE LABELS section from the .sps file
# Find where the VALUE LABELS section starts
start_idx <- grep("^VALUE LABELS", sps_lines)
if(length(start_idx) > 0) {
  labels_text <- sps_lines[(start_idx + 1):length(sps_lines)]
  
  # Combine into a single string for easier regex parsing
  labels_combined <- paste(labels_text, collapse = "\n")
  
  # Split by variables (each variable starts with a slash in the SPSS file)
  variables <- str_split(labels_combined, "\\n /")[[1]]
  
  # 4. Apply labels dynamically to the dataframe
  for (var_block in variables) {
    lines <- str_trim(str_split(var_block, "\\n")[[1]])
    lines <- lines[lines != "" & lines != "."]
    
    if (length(lines) < 2) next 
    
    # The first line is the variable name
    var_name <- str_replace_all(lines[1], "^/|/$", "")
    var_name <- str_trim(var_name)
    
    # Extract the value and the label mappings using regex
    mapping <- c()
    for (line in lines[-1]) {
      # Match pattern: '1' 'Label text'
      matches <- str_match(line, "^'([^']+)'\\s+'(.*)'$")
      if (!is.na(matches[1, 1])) {
        val <- as.numeric(matches[1, 2])
        lbl <- matches[1, 3]
        
        # Handle escaped quotes in SPSS syntax (e.g., NG''EVE -> NG'EVE)
        lbl <- str_replace_all(lbl, "''", "'") 
        
        mapping[lbl] <- val
      }
    }
    
    # If the variable exists in the dataframe, apply the haven::labelled mapping
    if (var_name %in% colnames(class_work_2_raw) && length(mapping) > 0) {
      class_work_2_raw[[var_name]] <- labelled(class_work_2_raw[[var_name]], mapping)
    }
  }
}

# 5. Convert all labelled variables to standard R factors
class_work_2_final <- as_factor(class_work_2_raw)

rm(list = c("labels_text", "line", "lines", "mapping", "val", "var_name"))

# View the reproducible result
View(class_work_2_final)

# ==============================================================================
# WORLD-CLASS DATA EXPLORATION SCRIPT
# Objective: Detect, Document, and Diagnose (Do not modify the data)
# ==============================================================================
library(dplyr)
library(tidyr)
library(purrr)

# Assuming your labelled dataset is loaded as `class_work_2_final`

# ==============================================================================
# LEVEL 1: STRUCTURAL INTEGRITY & MISSINGNESS PROFILING
# Rule: Count what is missing before deciding what missing means.
# ==============================================================================

# 1.1 Dataset Footprint
# Verify if the sample size meets the calculated n=427 requirement.
dim(class_work_2_final) 

# ==============================================================================
# DUPLICATE EXPLORATION SCRIPT (Aligned with Chapter 7.34)
# Objective: Detect Exact Duplicates and ID Collisions without deleting data.
# ==============================================================================
library(dplyr)

# ------------------------------------------------------------------------------
# TYPE 1: EXACT (COMPLETE) DUPLICATES 
# Theory: Every single column matches perfectly. Usually a double-click submit error.
# ------------------------------------------------------------------------------

print("--- 1. AUDIT FOR EXACT (COMPLETE) ROW DUPLICATES ---")

# The duplicated() function scans across all 44 columns. 
# If a row is a 100% carbon copy of an earlier row, it gets flagged as TRUE.
exact_duplicates <- class_work_2_final[duplicated(class_work_2_final), ]

# We count how many exact carbon copies exist.
cat("Number of exact duplicate rows found:", nrow(exact_duplicates), "\n")

# If there are any, print them out for inspection.
if(nrow(exact_duplicates) > 0) {
  print(exact_duplicates)
}


# ------------------------------------------------------------------------------
# TYPE 2: PARTIAL / KEY-VARIABLE DUPLICATES (ID COLLISIONS)
# Theory: The ID is duplicated, but the answers might differ. 
# This indicates an enumerator wrote the same ID for two different women.
# ------------------------------------------------------------------------------

print("--- 2. AUDIT FOR DUPLICATE QUESTIONNAIRE IDs ---")

# We isolate just the ID_NUMBE column, count how many times each ID appears, 
# and filter to show only IDs that appear more than once (n > 1).
duplicate_id_summary <- class_work_2_final %>%
  count(ID_NUMBE) %>%
  filter(n > 1)

print(duplicate_id_summary)


# ------------------------------------------------------------------------------
# TYPE 3: DEEP INSPECTION (SIDE-BY-SIDE COMPARISON)
# Theory: We must look at the demographics of the duplicate IDs to decide 
# if it is a typing error (different women) or a true duplicate entity.
# ------------------------------------------------------------------------------

print("--- 3. SIDE-BY-SIDE INSPECTION OF DUPLICATE IDs ---")

# We pull all records that share an ID number and put them next to each other.
# We select key demographic variables to check if these are different women.
duplicate_id_inspection <- class_work_2_final %>%
  group_by(ID_NUMBE) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(ID_NUMBE) %>%
  select(ID_NUMBE, QNA1_AGE, QNA2_EDU, QNA3_REL, QNA4_MAR, QNC1_CURT)

print(duplicate_id_inspection, n = 20)
# ------------------------------------------------------------------------------
# PREVIEW OF CHAPTER 9 (DATA CLEANING HANDOFF)
# Below is how you WILL handle these once you verify them in the paper records.
# DO NOT RUN THIS AS PART OF EXPLORATION, this is just for your learning!
# ------------------------------------------------------------------------------

# IF you check the paper forms and confirm that ID 106 represents two different 
# women, you would manually fix the ID typing error like this:
# class_work_2_clean <- class_work_2_final %>%
#   mutate(ID_NUMBE = ifelse(ID_NUMBE == 106 & QNA1_AGE == 15, 9999, ID_NUMBE))

# IF you confirm that an ID is a true duplicate entry and want to keep only 
# the FIRST occurrence, you would use this dplyr command:
# class_work_2_clean <- class_work_2_final %>%
#   distinct(ID_NUMBE, .keep_all = TRUE)




# 1.3 Comprehensive Missingness Table
# Generates a variable-by-variable breakdown of missing data (n and %).
# This is critical for deciding if a variable needs imputation later.
missing_summary <- data.frame(
  variable = names(class_work_2_final),
  missing_n = colSums(is.na(class_work_2_final)),
  missing_percent = round(colMeans(is.na(class_work_2_final)) * 100, 2)
) %>%
  filter(missing_n > 0) %>%
  arrange(desc(missing_percent))


print("Missing Data Profile (Top Variables):")
head(missing_summary, 10)

# ==============================================================================
# LEVEL 2: ELIGIBILITY & NUMERICAL DISTRIBUTIONS
# Rule: Are observed values scientifically plausible for the target population?
# ==============================================================================

# 2.1 Age Eligibility Boundaries (15-49 years)
# The study population is restricted to Women of Reproductive Age (WRA).
# This flags anyone interviewed outside the permitted study range.
class_work_2_final %>%
  filter(QNA1_AGE < 15 | QNA1_AGE > 49) %>%
  select(ID_NUMBE, QNA1_AGE) -> age_violations
print("Eligibility Violations (Age outside 15-49):")
print(age_violations)

# 2.2 Distributional Assessment of Numerical Variables (Parity & Age)
# Compares Mean vs. Median to detect skewness, and checks Min/Max for extreme outliers.
class_work_2_final %>%
  summarise(
    Age_Mean = mean(QNA1_AGE, na.rm = TRUE),
    Age_Median = median(QNA1_AGE, na.rm = TRUE),
    Age_Min = min(QNA1_AGE, na.rm = TRUE),
    Age_Max = max(QNA1_AGE, na.rm = TRUE),
    
    Parity_Mean = mean(QNA7_NO, na.rm = TRUE),
    Parity_Median = median(QNA7_NO, na.rm = TRUE),
    Parity_Max = max(QNA7_NO, na.rm = TRUE) # Flags unusually high number of children
  ) %>%
  t() %>% print()

# ==============================================================================
# LEVEL 3: STRICT CROSS-VARIABLE CONSISTENCY (LOGIC CHECKS)
# Rule: Do related variables contradict one another based on the questionnaire?
# ==============================================================================

# 3.1 Skip Pattern A: Marital Status vs. Partner Questions
# Section D (Q1-Q3) is ONLY for women currently in a union.
# This flags unmarried/divorced women who have answers for husband support/decision-making.
class_work_2_final %>%
  filter(QNA4_MAR == "Not in union" & 
           (!is.na(QND1_HUSH) | !is.na(QND2_DEC4) | !is.na(QND3_COM))) %>%
  select(ID_NUMBE, QNA4_MAR, QND1_HUSH, QND2_DEC4) -> skip_pattern_marital
print("Skip Pattern Violations (Marital Status):")
print(skip_pattern_marital)

# 3.2 Skip Pattern B: Contraceptive Use vs. Method Specified
# If a woman is NOT using contraceptives (QNC1_CURT == "No"), 
# she should NOT have a specific method recorded in QNC2_CON.
class_work_2_final %>%
  filter(QNC1_CURT == "No" & !is.na(QNC2_CON)) %>%
  select(ID_NUMBE, QNC1_CURT, QNC2_CON) -> skip_pattern_usage
print("Skip Pattern Violations (Contraceptive Usage):")
print(skip_pattern_usage)

# 3.3 Biological Consistency: Age vs. Parity
# Flags highly improbable biological scenarios (e.g., very young age with high parity).
class_work_2_final %>%
  filter((QNA1_AGE < 18 & QNA7_NO >= 3) | (QNA1_AGE < 20 & QNA7_NO >= 4)) %>%
  select(ID_NUMBE, QNA1_AGE, QNA7_NO) -> bio_logic_flags
print("Biological Logic Flags (Age vs. Parity):")
print(bio_logic_flags)

# ==============================================================================
# LEVEL 4: KNOWLEDGE SCORE BASELINE VERIFICATION
# Rule: Before computing a composite score, ensure the components are intact.
# ==============================================================================

# 4.1 Knowledge Component Missingness
# The proposal defines an 18-point knowledge score derived from Section B.
# If a respondent has missing data across these columns, their final score will be NA.
knowledge_vars <- c("QNB1A_CO", "QNB1B_CO", "QNB1C_CO", "QNB1D_CO", "QNB1E_CO", "QNB1F_CO",
                    "QNB2A_ADY", "QNB2B_ADK", "QNB2C_ADR", "QNB2D_ADM", "QNB2E_ADI", 
                    "QNB2F_ADP", "QNB2G_ADP", "QNB2H_ADe",
                    "QNB3A_SIh", "QNB3B_SIS", "QNB3C_SIQ", "QNB3D_SIL")

class_work_2_final %>%
  select(ID_NUMBE, all_of(knowledge_vars)) %>%
  mutate(missing_knowledge_items = rowSums(is.na(select(., -ID_NUMBE)))) %>%
  filter(missing_knowledge_items > 0) %>%
  select(ID_NUMBE, missing_knowledge_items) -> incomplete_knowledge
print("Respondents with Missing Knowledge Score Components:")
head(incomplete_knowledge)

# ==============================================================================
# LEVEL 5: CLINICALLY MEANINGFUL STRATIFICATION
# Rule: Single overall summaries hide important structural patterns.
# ==============================================================================

# 5.1 Outcome (Contraceptive Use) by Key Socio-Cultural Groups
# Exploring how the primary outcome distributes across Religion and Marital Status.
print("Contraceptive Use Stratified by Religion:")
table(class_work_2_final$QNA3_REL, class_work_2_final$QNC1_CURT, useNA = "ifany")

print("Contraceptive Use Stratified by Partner Support (Married Only):")
class_work_2_final %>%
  filter(QNA4_MAR == "Married/Cohabited") %>%
  count(QND1_HUSH, QNC1_CURT) %>%
  pivot_wider(names_from = QNC1_CURT, values_from = n) %>% print()

# ==============================================================================
# END OF EXPLORATION
# Next Step: Log all findings from above, verify against raw questionnaires, 
# and move to Chapter 9 Data Cleaning.
# ==============================================================================






# ==============================================================================
# CHAPTER 8: VISUAL EXPLORATION SCRIPT
# Goal: Make the truth in the data easier to see before cleaning.
# ==============================================================================

# R uses a "Grammar of Graphics" approach (ggplot2), building plots step-by-step:
# DATA + AESTHETICS + GEOMETRY + SCALES + THEME.

library(ggplot2)
# install.packages("naniar") # Run once if not installed
library(naniar)
library(dplyr)

# ==============================================================================
# 1. VISUALIZING MISSINGNESS (Diagnostic)
# Question: How does missingness occur? -> Missingness Plot
# ==============================================================================

# Missingness is itself a pattern[cite: 3].
# This creates a visual map showing exactly where missing data (NAs) are clustered.
# For your dataset, you should expect large blocks of missing data for partner questions
# because unmarried women skipped them[cite: 3].

vis_miss(class_work_2_final)

# ==============================================================================
# 2. ONE CATEGORICAL VARIABLE (Exploratory)
# Question: How are categories distributed? -> Bar Chart[cite: 3]
# ==============================================================================

# We visualize 'QNA2_EDU' (Education Level).
# geom_bar() counts the number of observations in each category[cite: 3].
# We use clear axis labels so the reader does not have to guess units or meaning[cite: 3].

ggplot(class_work_2_final, aes(x = QNA2_EDU)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Education Levels",
    x = "Highest Education Reached",
    y = "Count (Number of Women)"
  ) +
  theme_minimal()

# ==============================================================================
# 3. ONE NUMERICAL VARIABLE (Exploratory & Diagnostic)
# Question: How is one numerical variable distributed? -> Histogram[cite: 3]
# ==============================================================================

# We look at 'QNA1_AGE'. 
# A histogram reveals symmetry, skewness, extreme tails, and heaping[cite: 3].
# We set bins = 25. Chapter 8 warns that bin width matters: too few hide structure, 
# too many create random noise[cite: 3].

ggplot(class_work_2_final, aes(x = QNA1_AGE)) +
  geom_histogram(binwidth = 1, fill = "darkorange", color = "black") + # Exactly 1 bar per year
  labs(
    title = "Age Distribution (1-Year Bins Exposing Rounding)",
    x = "Age (Years)",
    y = "Frequency"
  ) +
  theme_minimal()

# DIAGNOSTIC CHECK: Look for "heaping"[cite: 3]. Do you see unnatural spikes 
# at ages 20, 25, 30, and 35? This suggests women or enumerators were rounding ages.

# ==============================================================================
# 4. NUMERICAL × CATEGORICAL (Group Comparison)
# Question: How does a numerical variable differ across groups? -> Boxplot[cite: 3]
# ==============================================================================

# We compare 'QNA7_NO' (Number of living children / Parity) across 'QNC1_CURT' (Contraceptive use).
# A boxplot summarizes the median, IQR, and potential outliers[cite: 3].

ggplot(class_work_2_final, aes(x = QNC1_CURT, y = QNA7_NO)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Parity (Number of Children) by Contraceptive Use",
    x = "Currently Using Modern Contraceptives?",
    y = "Number of Living Children"
  ) +
  theme_minimal()
# DIAGNOSTIC CHECK: A point beyond the whisker is flagged as a potential outlier[cite: 3].
# It is NOT automatically an error; it just means it is unusually distant from the rest[cite: 3].

# ==============================================================================
# 5. NUMERICAL × NUMERICAL (Relationship)
# Question: Are numerical variables related? -> Scatterplot[cite: 3]
# ==============================================================================

# We plot Age (QNA1_AGE) against Parity (QNA7_NO).
# Because age and parity are whole numbers, many dots will stack on top of each other.
# This causes "overplotting," creating a dark cloud that hides the data[cite: 3].
# We solve this by using geom_jitter() instead of geom_point(), and adding transparency (alpha)[cite: 3].

ggplot(class_work_2_final, aes(x = QNA1_AGE, y = QNA7_NO)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.4, color = "purple") +
  labs(
    title = "Relationship Between Age and Number of Children",
    x = "Age (Years)",
    y = "Number of Living Children"
  ) +
  theme_minimal()
# DIAGNOSTIC CHECK: Do you see any young teenagers with 5+ children? 
# If so, you have found a biological implausibility that must be checked in Chapter 9[cite: 3].

# ==============================================================================
# 6. CATEGORICAL × CATEGORICAL (Composition)
# Question: How are two categories related? -> Proportional Bar Chart[cite: 3]
# ==============================================================================

# We look at Religion (QNA3_REL) vs Contraceptive Use (QNC1_CURT).
# We use position = "fill" so each bar represents 100% (proportions) rather than raw counts[cite: 3].
# This prevents groups with different sample sizes from distorting the comparison[cite: 3].

class_work_2_final %>%
  tidyr::drop_na(QNA3_REL, QNC1_CURT) %>% # Temporarily drop missing for clear visual
  ggplot(aes(x = QNA3_REL, fill = QNC1_CURT)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Contraceptive Users by Religion",
    x = "Religion",
    y = "Proportion (0 to 1)",
    fill = "Using Contraceptives?"
  ) +
  scale_y_continuous(labels = scales::percent) + # Make y-axis readable as %
  theme_minimal()




# ==============================================================================
# CHAPTER 9: REPRODUCIBLE DATA CLEANING SCRIPT
# Rule: Explore to discover. Clean to correct. Analyze to answer.
# ==============================================================================
library(dplyr)
library(stringr)

# ------------------------------------------------------------------------------
# STEP 1: PROTECT THE RAW DATA & ESTABLISH A BASELINE
# ------------------------------------------------------------------------------
# We never overwrite class_work_2_final. We create a working copy.
data_work <- class_work_2_final

View("--- BASELINE ---")
cat("Starting rows:", nrow(data_work), "\n")
cat("Starting columns:", ncol(data_work), "\n")


# ------------------------------------------------------------------------------
# STEP 2: STRUCTURAL CLEANING (Drop Empty Administrative Columns)[cite: 5]
# FINDING: 'note_knowledge', 'note_adv', and 'note_side' were 100% missing.
# DECISION: Drop them, as they are KoboToolbox structural notes, not data.
# ------------------------------------------------------------------------------
data_work <- data_work %>%
  select(-note_knowledge, -note_adv, -note_side)


# ------------------------------------------------------------------------------
# STEP 3: RESOLVING EXACT DUPLICATE ROWS[cite: 5]
# FINDING: IDs 194, 234, and 424 appeared as 100% exact carbon copies.
# DECISION: Remove the redundant copies using distinct().
# ------------------------------------------------------------------------------
data_work <- data_work %>%
  distinct() # This removes exact duplicate rows, keeping only one copy[cite: 5]


# ------------------------------------------------------------------------------
# STEP 4: RESOLVING ID COLLISIONS (Different Women, Same ID)[cite: 5]
# FINDING: IDs 106, 170, and 229 had conflicting ages/demographics, 424 had same age but conflicts in other.
# DECISION: Do NOT delete them[cite: 5]. Based on source verification of paper forms, 
# reassign the correct IDs to the second instances.
# ------------------------------------------------------------------------------
# Example transformation (Assuming paper forms confirmed the correct IDs):
data_work <- data_work %>%
  mutate(
    ID_NUMBE = case_when(
      ID_NUMBE == 106 & QNA1_AGE == 15 ~ 9106, # Reassigning the 15-year-old
      ID_NUMBE == 170 & QNA1_AGE == 26 ~ 9170, # Reassigning the 26-year-old
      ID_NUMBE == 229 & QNA1_AGE == 19 ~ 9229, # Reassigning the 19-year-old
      ID_NUMBE == 424 & duplicated(ID_NUMBE) ~ 9424, # Reassigning the second 36-year-old
      TRUE ~ ID_NUMBE
    )
  )



# ------------------------------------------------------------------------------
# STEP 5: RESOLVING SKIP-PATTERN VIOLATIONS (Logical Consistency)[cite: 5]
# FINDING: 4 "Not in union" women had answers for husband's support (QND1_HUSH)[cite: 5].
# DECISION: Force husband-related variables to NA for unmarried women to respect protocol[cite: 5].
# ------------------------------------------------------------------------------
data_work <- data_work %>%
  mutate(
    QND1_HUSH = if_else(QNA4_MAR == "Not in union", NA, QND1_HUSH),
    QND2_DEC4 = if_else(QNA4_MAR == "Not in union", NA, QND2_DEC4),
    QND3_COM  = if_else(QNA4_MAR == "Not in union", NA, QND3_COM)
  )


# ------------------------------------------------------------------------------
# STEP 6: VALIDATION (Did the cleaning work?)[cite: 5]
# ------------------------------------------------------------------------------
print("--- VALIDATION ---")

# 1. Structural Validation[cite: 5]: Did we drop the exact duplicates?
cat("Ending rows:", nrow(data_work), "(Expected: 421 - 2 exact duplicates = 419)\n")

# 2. Duplicate Validation[cite: 5]: Are there any repeated IDs left?
remaining_duplicates <- data_work %>% count(ID_NUMBE) %>% filter(n > 1)
cat("Remaining Duplicate IDs:", nrow(remaining_duplicates), "(Expected: 0)\n")

# 3. Logic Validation[cite: 5]: Do any unmarried women have husband support data?
skip_errors_left <- data_work %>% 
  filter(QNA4_MAR == "Not in union" & !is.na(QND1_HUSH)) %>% 
  nrow()
cat("Remaining Skip Pattern Violations:", skip_errors_left, "(Expected: 0)\n")


# ------------------------------------------------------------------------------
# STEP 7: SAVE ANALYSIS-READY DATASET[cite: 5]
# We save the cleaned dataset separately, leaving the raw data untouched[cite: 5].
# ------------------------------------------------------------------------------
# saveRDS(data_work, "data/class_work_2_clean.rds")
library(readr)
write_csv(data_work,"data/groupE_cleaned.csv")

# ==============================================================================
# CHAPTER 11 & PROPOSAL ALIGNED SCRIPT (UPDATED FOR groupE_cleaned.csv)
# ==============================================================================
library(dplyr)
library(readr)

# 1. Load your cleaned dataset
data_clean <- read_csv("data/groupE_cleaned.csv")

library(dplyr)
library(readr)

# ------------------------------------------------------------------------------
# 1. LOAD DATA

# ==============================================================================
# TAFITI99 — CATEGORICAL ANALYSIS ENGINE
# Research: Group E
# Outcome: QNC1_CURT
#
# Output for every categorical analysis:
#   1. Observed frequencies (n)
#   2. Row percentages (%)
#   3. Row and column marginal totals
#   4. Expected frequencies
#   5. Chi-square assumption check
#   6. Pearson Chi-square / Fisher's Exact / Monte Carlo test
#   7. Test statistic, df and p-value
# ==============================================================================


# ==============================================================================
# 1. LOAD REQUIRED PACKAGES
# ==============================================================================

library(dplyr)
library(readr)


# ==============================================================================
# 2. LOAD CLEANED DATA
# ==============================================================================

data_clean <- read_csv(
  "data/groupE_cleaned.csv",
  show_col_types = FALSE
)


# ==============================================================================
# 3. DERIVE ANALYTICAL VARIABLES
# ==============================================================================

data_clean <- data_clean %>%
  mutate(
    
    # --------------------------------------------------------------------------
    # OBJ3-A: SOCIO-DEMOGRAPHIC VARIABLES
    # --------------------------------------------------------------------------
    
    Age_Group_3cat = case_when(
      QNA1_AGE >= 15 & QNA1_AGE <= 24 ~ "15-24 years",
      QNA1_AGE >= 25 & QNA1_AGE <= 34 ~ "25-34 years",
      QNA1_AGE >= 35 & QNA1_AGE <= 49 ~ "35-49 years",
      TRUE ~ NA_character_
    ),
    
    Education_Binary = case_when(
      QNA2_EDU %in% c("No education", "Primary") ~
        "Primary or less",
      
      QNA2_EDU %in% c("Secondary", "Higher") ~
        "Secondary or higher",
      
      TRUE ~ NA_character_
    ),
    
    Parity_Binary = case_when(
      QNA7_NO >= 0 & QNA7_NO <= 2 ~
        "0-2 children",
      
      QNA7_NO >= 3 ~
        "3 or more children",
      
      TRUE ~ NA_character_
    ),
    
    
    # --------------------------------------------------------------------------
    # OBJ3-C: SOCIO-CULTURAL MYTHS & CULTURE
    # --------------------------------------------------------------------------
    
    Myth_Infertility_Binary = case_when(
      QND4_MYTA == "Yes" ~
        "Agree (myth endorsed)",
      
      QND4_MYTA %in% c("No", "I dont know", "Dont Know") ~
        "Disagree or Don't Know",
      
      TRUE ~ NA_character_
    ),
    
    Myth_Cancer_Binary = case_when(
      QND5_MYT2 == "Yes" ~
        "Agree (myth endorsed)",
      
      QND5_MYT2 %in% c("No", "I dont know", "Dont Know") ~
        "Disagree or Don't Know",
      
      TRUE ~ NA_character_
    ),
    
    Myth_Infidelity_Binary = case_when(
      QND6_MYTG == "Yes" ~
        "Agree (myth endorsed)",
      
      QND6_MYTG %in% c("No", "I dont know", "Dont Know") ~
        "Disagree or Don't Know",
      
      TRUE ~ NA_character_
    ),
    
    Cultural_Value_Binary = case_when(
      QND7_MANT == "Yes" ~
        "Agree (children = wealth/prestige)",
      
      QND7_MANT %in% c("No", "I dont know", "Dont Know") ~
        "Disagree or Don't Know",
      
      TRUE ~ NA_character_
    ),
    
    Religious_Binary = case_when(
      QND8_REL3 == "Yes" ~
        "Religion allows contraceptive use",
      
      QND8_REL3 %in% c("No", "I dont know", "Dont Know") ~
        "Not allowed or Don't Know",
      
      TRUE ~ NA_character_
    ),
    
    
    # --------------------------------------------------------------------------
    # OBJ3-C: PARTNER DYNAMICS
    # --------------------------------------------------------------------------
    
    Partner_Support_Binary = case_when(
      QND1_HUSH == "Yes" ~
        "Supportive",
      
      QND1_HUSH %in% c("No", "Dont Know", "I dont know") ~
        "Not supportive or Don't Know",
      
      TRUE ~ NA_character_
    ),
    
    Decision_Binary = case_when(
      QND2_DEC4 %in% c(
        "Joint decision",
        "Mainly respondent"
      ) ~
        "Joint decision or woman alone",
      
      QND2_DEC4 %in% c(
        "Mainly husband/partner",
        "Name the other"
      ) ~
        "Mainly husband/partner or other",
      
      TRUE ~ NA_character_
    )
  )


# ==============================================================================
# 4. KNOWLEDGE SCORE
# ==============================================================================

data_clean <- data_clean %>%
  rowwise() %>%
  mutate(
    knowledge_score = sum(
      c_across(
        c(
          QNB1A_CO:QNB1F_CO,
          QNB2A_ADY:QNB2H_ADe,
          QNB3A_SIh:QNB3D_SIL
        )
      ) == "Yes",
      na.rm = TRUE
    )
  ) %>%
  ungroup()


# ==============================================================================
# 5. KNOWLEDGE LEVEL
# ==============================================================================

mean_score <- mean(
  data_clean$knowledge_score,
  na.rm = TRUE
)

data_clean <- data_clean %>%
  mutate(
    Knowledge_Level = if_else(
      knowledge_score >= mean_score,
      "High Knowledge",
      "Low Knowledge"
    ),
    
    Knowledge_Level = factor(
      Knowledge_Level,
      levels = c(
        "Low Knowledge",
        "High Knowledge"
      )
    )
  )


# ==============================================================================
# 6. CATEGORICAL ANALYSIS ENGINE
# ==============================================================================

analyze_categorical <- function(
    data,
    predictor,
    outcome = "QNC1_CURT"
) {
  
  # --------------------------------------------------------------------------
  # STEP 1: KEEP ONLY COMPLETE CASES
  # --------------------------------------------------------------------------
  
  analysis_data <- data[
    complete.cases(
      data[, c(predictor, outcome)]
    ),
  ]
  
  n_total <- nrow(data)
  n_analyzed <- nrow(analysis_data)
  n_excluded <- n_total - n_analyzed
  
  
  # --------------------------------------------------------------------------
  # STEP 2: CREATE OBSERVED CONTINGENCY TABLE
  # --------------------------------------------------------------------------
  
  tab <- table(
    analysis_data[[predictor]],
    analysis_data[[outcome]]
  )
  
  
  # --------------------------------------------------------------------------
  # STEP 3: ADD MARGINAL TOTALS
  # --------------------------------------------------------------------------
  
  tab_with_totals <- addmargins(tab)
  
  
  # --------------------------------------------------------------------------
  # STEP 4: CALCULATE ROW PERCENTAGES
  # --------------------------------------------------------------------------
  
  row_percent <- prop.table(
    tab,
    margin = 1
  ) * 100
  
  
  # --------------------------------------------------------------------------
  # STEP 5: CREATE PUBLICATION-STYLE n (%)
  # --------------------------------------------------------------------------
  
  display_table <- matrix(
    "",
    nrow = nrow(tab) + 1,
    ncol = ncol(tab) + 1
  )
  
  rownames(display_table) <- c(
    rownames(tab),
    "Total"
  )
  
  colnames(display_table) <- c(
    colnames(tab),
    "Total"
  )
  
  
  # --------------------------------------------------------------------------
  # STEP 6: FILL CELL VALUES WITH n (%)
  # --------------------------------------------------------------------------
  
  for (i in seq_len(nrow(tab))) {
    
    for (j in seq_len(ncol(tab))) {
      
      display_table[i, j] <- paste0(
        tab[i, j],
        " (",
        sprintf("%.1f", row_percent[i, j]),
        "%)"
      )
    }
    
    # Every individual row totals 100%
    display_table[i, ncol(tab) + 1] <- paste0(
      sum(tab[i, ]),
      " (100.0%)"
    )
  }
  
  
  # --------------------------------------------------------------------------
  # STEP 7: TOTAL ROW
  #
  # For the total row, percentages are overall percentages because
  # there is no meaningful "row percentage" within the total row.
  # --------------------------------------------------------------------------
  
  overall_percent <- prop.table(tab) * 100
  
  for (j in seq_len(ncol(tab))) {
    
    display_table[nrow(tab) + 1, j] <- paste0(
      sum(tab[, j]),
      " (",
      sprintf("%.1f", overall_percent[j]),
      "%)"
    )
  }
  
  display_table[
    nrow(tab) + 1,
    ncol(tab) + 1
  ] <- paste0(
    sum(tab),
    " (100.0%)"
  )
  
  
  # --------------------------------------------------------------------------
  # STEP 8: INITIAL CHI-SQUARE TEST
  #
  # Used to inspect expected cell frequencies.
  # --------------------------------------------------------------------------
  
  chi_raw <- suppressWarnings(
    chisq.test(
      tab,
      correct = FALSE
    )
  )
  
  expected <- chi_raw$expected
  
  
  # --------------------------------------------------------------------------
  # STEP 9: CHECK CHI-SQUARE ASSUMPTIONS
  # --------------------------------------------------------------------------
  
  expected_less_than_1 <- sum(
    expected < 1
  )
  
  expected_less_than_5 <- sum(
    expected < 5
  )
  
  total_cells <- length(expected)
  
  percent_less_than_5 <- (
    expected_less_than_5 /
      total_cells
  ) * 100
  
  
  is_sparse <- (
    expected_less_than_1 > 0 ||
      percent_less_than_5 > 20
  )
  
  
  # --------------------------------------------------------------------------
  # STEP 10: SELECT APPROPRIATE TEST
  # --------------------------------------------------------------------------
  
  if (!is_sparse) {
    
    method <- "Pearson Chi-Square"
    
    result <- chisq.test(
      tab,
      correct = FALSE
    )
    
  } else if (
    nrow(tab) == 2 &&
    ncol(tab) == 2
  ) {
    
    method <- "Fisher's Exact Test"
    
    result <- fisher.test(tab)
    
  } else {
    
    method <- "Monte Carlo Chi-Square"
    
    set.seed(12345)
    
    result <- chisq.test(
      tab,
      simulate.p.value = TRUE,
      B = 10000
    )
  }
  
  
  # =============================================================================
  # 11. DISPLAY RESULTS
  # =============================================================================
  
  cat("\n")
  cat("====================================================================\n")
  cat("CATEGORICAL ANALYSIS\n")
  cat("====================================================================\n")
  
  cat(
    "Predictor :", predictor, "\n"
  )
  
  cat(
    "Outcome   :", outcome, "\n"
  )
  
  cat(
    "Total N   :", n_total, "\n"
  )
  
  cat(
    "Analyzed N:", n_analyzed, "\n"
  )
  
  cat(
    "Excluded N:", n_excluded, "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # OBSERVED n (%)
  # --------------------------------------------------------------------------
  
  cat("\n")
  cat("OBSERVED FREQUENCIES WITH ROW PERCENTAGES\n")
  cat("--------------------------------------------------------------------\n")
  
  print(
    as.data.frame.matrix(display_table)
  )
  
  
  # --------------------------------------------------------------------------
  # RAW MARGINAL TOTALS
  # --------------------------------------------------------------------------
  
  cat("\n")
  cat("MARGINAL TOTALS — RAW COUNTS\n")
  cat("--------------------------------------------------------------------\n")
  
  print(tab_with_totals)
  
  
  # --------------------------------------------------------------------------
  # EXPECTED COUNTS
  # --------------------------------------------------------------------------
  
  cat("\n")
  cat("EXPECTED FREQUENCIES\n")
  cat("--------------------------------------------------------------------\n")
  
  print(
    round(expected, 2)
  )
  
  
  # --------------------------------------------------------------------------
  # ASSUMPTION CHECK
  # --------------------------------------------------------------------------
  
  cat("\n")
  cat("CHI-SQUARE ASSUMPTION CHECK\n")
  cat("--------------------------------------------------------------------\n")
  
  cat(
    "Cells with expected count < 1 :",
    expected_less_than_1,
    "\n"
  )
  
  cat(
    "Cells with expected count < 5 :",
    expected_less_than_5,
    "of",
    total_cells,
    "\n"
  )
  
  cat(
    "Percentage of cells < 5      :",
    sprintf("%.1f%%", percent_less_than_5),
    "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # SELECTED TEST
  # --------------------------------------------------------------------------
  
  cat("\n")
  cat("SELECTED STATISTICAL TEST\n")
  cat("--------------------------------------------------------------------\n")
  
  cat(
    method,
    "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # STATISTICAL RESULT
  # --------------------------------------------------------------------------
  
  cat("\n")
  cat("TEST RESULT\n")
  cat("--------------------------------------------------------------------\n")
  
  print(result)
  
  
  cat("====================================================================\n")
  
  
  # =============================================================================
  # 12. RETURN RESULTS FOR FUTURE EXPORT
  # =============================================================================
  
  invisible(
    list(
      predictor = predictor,
      outcome = outcome,
      
      n_total = n_total,
      n_analyzed = n_analyzed,
      n_excluded = n_excluded,
      
      observed = tab,
      observed_with_totals = tab_with_totals,
      
      row_percent = row_percent,
      
      display_table = display_table,
      
      expected = expected,
      
      expected_less_than_1 = expected_less_than_1,
      expected_less_than_5 = expected_less_than_5,
      percent_less_than_5 = percent_less_than_5,
      
      method = method,
      test = result
    )
  )
}


# ==============================================================================
# 13. RUN ANALYSIS — SOCIO-DEMOGRAPHIC FACTORS
# ==============================================================================

cat("\n\n")
cat("####################################################################\n")
cat("# SOCIO-DEMOGRAPHIC FACTORS\n")
cat("####################################################################\n")

analyze_categorical(
  data_clean,
  predictor = "Age_Group_3cat"
)

analyze_categorical(
  data_clean,
  predictor = "Education_Binary"
)

analyze_categorical(
  data_clean,
  predictor = "Parity_Binary"
)


# ==============================================================================
# 14. RUN ANALYSIS — KNOWLEDGE
# ==============================================================================

cat("\n\n")
cat("####################################################################\n")
cat("# KNOWLEDGE FACTORS\n")
cat("####################################################################\n")

analyze_categorical(
  data_clean,
  predictor = "Knowledge_Level"
)


# ==============================================================================
# 15. RUN ANALYSIS — SOCIO-CULTURAL FACTORS
# ==============================================================================

cat("\n\n")
cat("####################################################################\n")
cat("# SOCIO-CULTURAL: MYTHS & CULTURE — ALL WOMEN\n")
cat("####################################################################\n")

analyze_categorical(
  data_clean,
  predictor = "Myth_Infertility_Binary"
)

analyze_categorical(
  data_clean,
  predictor = "Myth_Cancer_Binary"
)

analyze_categorical(
  data_clean,
  predictor = "Myth_Infidelity_Binary"
)

analyze_categorical(
  data_clean,
  predictor = "Cultural_Value_Binary"
)

analyze_categorical(
  data_clean,
  predictor = "Religious_Binary"
)


# ==============================================================================
# 16. MARRIED/COHABITING WOMEN ONLY
# ==============================================================================

married_women <- data_clean %>%
  filter(
    QNA4_MAR == "Married/Cohabited"
  )


cat("\n\n")
cat("####################################################################\n")
cat("# SOCIO-CULTURAL: PARTNER DYNAMICS — MARRIED/COHABITING WOMEN\n")
cat("####################################################################\n")


analyze_categorical(
  married_women,
  predictor = "Partner_Support_Binary"
)

analyze_categorical(
  married_women,
  predictor = "Decision_Binary"
)

analyze_categorical(
  married_women,
  predictor = "QND3_COM"
)


# ==============================================================================
# END OF ANALYSIS
# ==============================================================================