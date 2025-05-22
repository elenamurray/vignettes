library(lme4)
library(lmerTest)
library(modelsummary)
library(dplyr)
library(kableExtra)
library(tidyr)
library(knitr)
library(tibble)
library(ggplot2)
library(gt)
library(gtExtras)

#Load data
load("/Users/elenamurray/Documents/Documents/HERTIE MASTERS/Semester 4/Thesis/build-vignettes-elena/vignettes/analysis_data.RData")

###----Combined variables
#Check cronbachs alpha 
#FRT Concern
cronbach_FRT_concern <- alpha(analysis_data[, c("FRT_base_1", "FRT_base_2", "FRT_base_3")])

#AI literacy scale
cronbach_AI_Lit <- alpha(analysis_data[, c("AI_lit_1", "AI_lit_2", "AI_lit_3", "AI_lit_4")])

#overall acceptance responses
cronbach_overall_acc <- alpha(analysis_data[, c("supp", "trust", "behav")])

###---- Filtering and Conditions
#Create summary for different conditions of t for vignette pages and manipulation checks
conditions_summary <- analysis_data %>%
  group_by(ResponseId) %>%
  summarise(
    any_t_under_5 = any(t_vig_page_submit < 5),
    fail_mani_1 = any(mani_scenario_check == "False"),
    fail_mani_2 = any(mani_variables_check == "False")
  ) %>%
  mutate(
    fail_any_mani = fail_mani_1 | fail_mani_2,
    fail_exactly_one = xor(fail_mani_1, fail_mani_2),
    fail_both_mani = fail_mani_1 & fail_mani_2,
    fail_mani_1_only = fail_mani_1 & !fail_mani_2,
    fail_mani_2_only = fail_mani_2 & !fail_mani_1,
    t_under_5_and_fail_any = any_t_under_5 & (fail_mani_1 | fail_mani_2),
    t_under_5_and_fail_both = any_t_under_5 & fail_both_mani,
    t_under_5_and_fail_exactly_one = any_t_under_5 & fail_exactly_one,
    t_under_5_and_pass_both = any_t_under_5 & !(fail_mani_1 | fail_mani_2)
  )

#Count cases
conditions_summary_table <- tibble(
  condition = c(
    "Had at least one vignette page with t < 5",
    "Failed manipulation check 1",
    "Failed manipulation check 2",
    "Failed at least one manipulation check",
    "Failed exactly one manipulation check",
    "Failed both manipulation checks",
    "Failed check 1 but not 2",
    "Failed check 2 but not 1",
    "Had at least one vignette page with t < 5 and failed at least one check",
    "Had at least one vignette page with t < 5 and failed both checks",
    "Had at least one vignette page with t < 5 and failed exactly one check",
    "Had at least one vignette page with t < 5 and passed both checks"
  ),
  count = c(
    sum(conditions_summary$any_t_under_5),
    sum(conditions_summary$fail_mani_1),
    sum(conditions_summary$fail_mani_2),
    sum(conditions_summary$fail_any_mani),
    sum(conditions_summary$fail_exactly_one),
    sum(conditions_summary$fail_both_mani),
    sum(conditions_summary$fail_mani_1_only),
    sum(conditions_summary$fail_mani_2_only),
    sum(conditions_summary$t_under_5_and_fail_any),
    sum(conditions_summary$t_under_5_and_fail_both),
    sum(conditions_summary$t_under_5_and_fail_exactly_one),
    sum(conditions_summary$t_under_5_and_pass_both)
  )
)

print(conditions_summary_table)

#Show in a table 
conditions_summary_table %>%
  kbl(
    col.names = c("Condition", "Number of Respondents"), 
    digits = 0
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped")
  ) %>%
  save_kable("conditions_summary_table.html")

# Create and reshape summary
time_summary <- analysis_data %>%
  summarise(
    `Mean survey duration (in minutes)` = mean(duration_in_minutes),
    `Median survey duration (in minutes)` = median(duration_in_minutes),
    `Mean info page time (in seconds)` = mean(t_info_page_submit),
    `Median info page time (in seconds)` = median(t_info_page_submit),
    `Mean vignette page time (in seconds)` = mean(t_vig_page_submit),
    `Median vignette page time (in seconds)` = median(t_vig_page_submit)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Measure", values_to = "Time")

#Show in a table 
time_summary %>%
  kbl(col.names = c("Measure", "Time"), digits = 2) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped")) %>% 
  save_kable(file = "time_summary_table.html")

##Create different groups to test
#Highest quality clean data: filter out respondents who failed either manipulation check OR had any vig page t < 5:
data_excl_mani_or_fast <- analysis_data %>%
  filter(!ResponseId %in% (
    conditions_summary %>%
      filter(fail_any_mani | any_t_under_5) %>%
      pull(ResponseId)
  ))

#Filter out only respondents who failed either manipulation check (regardless of t)
data_excl_any_mani_fail <- analysis_data %>%
filter(!ResponseId %in% (
  conditions_summary %>%
    filter(fail_any_mani) %>%
    pull(ResponseId)
))

#Filter out respondents who failed both manipulation checks and anyone with any t_vig_page_submit < 5
data_excl_both_mani_or_fast <- analysis_data %>%
  filter(!ResponseId %in% (
    conditions_summary %>%
      filter(fail_both_mani | any_t_under_5) %>%
      pull(ResponseId)
  ))

#Filter out only respondents who failed check 1
data_excl_mani_1_failures <- analysis_data %>%
  filter(!ResponseId %in% (
    conditions_summary %>%
      filter(fail_mani_1) %>%
      pull(ResponseId)
  ))

#Filter out only respondents who failed both manipulation checks
data_excl_both_mani_failures <- analysis_data %>%
  filter(!ResponseId %in% (
    conditions_summary %>%
      filter(fail_both_mani) %>%
      pull(ResponseId)
  ))

# Filter out respondents who had any vignette timing < 5 seconds
data_excl_fast_respondents <- analysis_data %>%
  filter(!ResponseId %in% (
    conditions_summary %>%
      filter(any_t_under_5) %>%
      pull(ResponseId)
  ))

# Count total respondents
total_respondents <- length(unique(analysis_data$ResponseId))

# Count respondents remaining in each filtered group
n_excl_mani_or_fast <- length(unique(data_excl_mani_or_fast$ResponseId))
n_excl_any_mani_fail <- length(unique(data_excl_any_mani_fail$ResponseId))
n_excl_both_mani_or_fast <- length(unique(data_excl_both_mani_or_fast$ResponseId))
n_excl_mani_1_failures <- length(unique(data_excl_mani_1_failures$ResponseId))
n_excl_both_mani_failures <- length(unique(data_excl_both_mani_failures$ResponseId))
n_excl_fast_respondents <- length(unique(data_excl_fast_respondents$ResponseId))  

# Summarise in a table
filter_summary <- tibble(
  Filter_Description = c(
    "Failed any manipulation check OR any t_vig_page_submit < 5",
    "Failed any manipulation check",
    "Failed both checks OR any t_vig_page_submit < 5",
    "Failed manipulation check 1",
    "Failed both manipulation checks",
    "Any t_vig_page_submit < 5"
  ),
  Respondents_Remaining = c(
    n_excl_mani_or_fast,
    n_excl_any_mani_fail,
    n_excl_both_mani_or_fast,
    n_excl_mani_1_failures,
    n_excl_both_mani_failures,
    n_excl_fast_respondents
  ),
  Percent_Removed = round(
    100 * (1 - c(
      n_excl_mani_or_fast,
      n_excl_any_mani_fail,
      n_excl_both_mani_or_fast,
      n_excl_mani_1_failures,
      n_excl_both_mani_failures,
      n_excl_fast_respondents
    ) / total_respondents),
    1
  )
)

# Display table
filter_summary %>%
  kbl(col.names = c("Filter Condition", "Respondents Remaining", "Percent Removed"), digits = 1) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped")) %>%
  save_kable(file = "filter_summary_table.html")

###-----Descriptive Data
# Select demographic variables
demo_vars <- analysis_data %>%
  select(ResponseId, Age = age, Gender = gender, Education = education, State = state, Urban = urban_rural, Minority = minority, Ideology = ideology) %>%
  distinct(ResponseId, .keep_all = TRUE)

# Create a summary table
demo_summary <- demo_vars %>%
  pivot_longer(cols = -ResponseId, names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(Number = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(`Percentage (%)` = round(100 * Number / sum(Number), 2)) %>%
  select(Variable, Category, Number, `Percentage (%)`)

# Print clean table without packed rows
demo_table <- kbl(demo_summary[, -1],
                  col.names = c("Category", "Number", "Percentage (%)"),
                  caption = "Distribution of Demographic Variables") %>%
  kable_paper("striped", full_width = FALSE) %>% 
  pack_rows("Age", 1, 6) %>%
  pack_rows("Education", 7, 11) %>% 
  pack_rows("Gender", 12, 14) %>%
  pack_rows("Ideology", 15, 19) %>% 
  pack_rows("Minority", 20, 21) %>% 
  pack_rows("State", 22, 29) %>% 
  pack_rows("Urban", 30, 32) %>%
  save_kable(file = "demo_summary_table.html")


##----Distributions of composite Likert scale responses
#Create summary data with distribution, mean, median, min, max
dist_data <- analysis_data %>%
  distinct(ResponseId, .keep_all = TRUE) %>%
  summarise(
    AI_lit = list(AI_lit),
    FRT_concern = list(FRT_concern),
    FRT_acceptance_composite = list(FRT_acceptance_composite)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Values") %>%
  rowwise() %>%
  mutate(
    Mean = round(mean(unlist(Values), na.rm = TRUE), 2),
    Median = round(median(unlist(Values), na.rm = TRUE), 2),
    Min = min(unlist(Values), na.rm = TRUE),
    Max = max(unlist(Values), na.rm = TRUE)
  ) %>%
  ungroup()

# Generate the GT table with histograms + summary stats
dist_table <- dist_data %>%
  gt() %>%
  gt_plt_dist(column = Values, type = "histogram", fill = "steelblue", line_color = "black") %>%
  cols_label(
    Variable = "Measure",
    Values = "Distribution",
    Mean = "Mean",
    Median = "Median",
    Min = "Min",
    Max = "Max"
  ) %>%
  tab_header(
    title = "Distribution of Composite Measures"
  )

# Save the table
gtsave(dist_table, "dist_table.html")

#-------Check that the vignettes and variables were balanced 
# Combine and calculate percentage within each vignette attribute
vignette_summary <- bind_rows(
  analysis_data %>% count(Variable = "Scenario", Category = scenario),
  analysis_data %>% count(Variable = "Actor", Category = actor),
  analysis_data %>% count(Variable = "Type", Category = type),
  analysis_data %>% count(Variable = "Scale", Category = scale)
) %>%
  group_by(Variable) %>%
  mutate(Percentage = round(100 * n / sum(n), 1)) %>%
  arrange(Variable, desc(n)) %>%
  ungroup()

# Create and display table using kableExtra
vignette_summary %>%
  kbl(
    col.names = c("Variable", "Category", "Count", "Percentage (%)"),
    caption = "Balance of Vignette Variables"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped")) %>%
  save_kable("vignette_balance_table.html")


###------Data Analysis & Hypothesis Testing------
#Model 1: Effect of Actor on Surveillance Technology Acceptance
model_1 <- lmer(FRT_acceptance_composite ~ actor + (1 | prolific_id), data = analysis_data)
summary(model_1)

#Model 2: Actor × Scenario Interaction Predicting FRT Acceptance
model_2 <- lmer(FRT_acceptance_composite ~ actor + scenario + actor * scenario + (1 | prolific_id), data = data_excl_mani_or_fast)
summary(model_2)

#Model 3: Minority + acceptance 
model_3 <- lmer(FRT_acceptance_composite ~ minority + (1 | prolific_id), data = data_excl_mani_or_fast)
summary(model_3)

#Model 4: Type + acceptance 
model_4 <- lmer(FRT_acceptance_composite ~ type + (1 | prolific_id), data = data_excl_mani_or_fast)
summary(model_4)
