## Individual plots
#Distribution plot - overall acceptance - Density
ggplot(analysis_data %>% distinct(ResponseId, .keep_all = TRUE),
       aes(x = FRT_acceptance_composite)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(
    title = "Distribution of FRT Acceptance Composite Score",
    x = "FRT Acceptance Composite Score",
    y = "Number of Respondents"
  ) +
  theme_minimal()+ 
  geom_vline(aes(xintercept = mean(FRT_acceptance_composite)),
             color = "black", linetype = "dashed", linewidth = 1)

#Distribution plot - overall acceptance - Histogram
ggplot(analysis_data %>% distinct(ResponseId, .keep_all = TRUE),
       aes(x = FRT_acceptance_composite)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 10, alpha = 0.6) +
  labs(
    title = "Distribution of FRT Acceptance Composite Score",
    x = "FRT Acceptance Composite Score",
    y = "Number of Respondents"
  ) +
  theme_minimal()+ 
  geom_vline(aes(xintercept = mean(FRT_acceptance_composite)),
             color = "black", linetype = "dashed", linewidth = 1)


#AI Lit plot 
ggplot(analysis_data %>% distinct(ResponseId, .keep_all = TRUE),
       aes(x = AI_lit)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 10, alpha = 0.6) +
  labs(
    title = "Distribution of AI Literacy Score",
    x = "AI Literacy Score",
    y = "Number of Respondents"
  ) +
  theme_minimal() + 
  geom_vline(aes(xintercept = mean(AI_lit)),
             color = "black", linetype = "dashed", linewidth = 1)

#FRT concern plot 
ggplot(analysis_data %>% distinct(ResponseId, .keep_all = TRUE),
       aes(x = FRT_concern)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 10, alpha = 0.6) +
  labs(
    title = "Distribution of FRT Concern Score",
    x = "FRT Concern Score",
    y = "Number of Respondents"
  ) +
  theme_minimal() + 
  geom_vline(aes(xintercept = mean(FRT_concern)),
             color = "black", linetype = "dashed", linewidth = 1)


#-------Models from first try:

# Linear mixed-effects model with fixed effects for predictors and random intercepts for respondent and vignette deck.
model_mixed_effects <- lmer(
  supp ~ actor + scenario + type + scale + AI_lit + FRT_concern + age + gender + education + state + urban_rural + minority + ideology +
    (1 | prolific_id) + (1 | deck_id), 
  data = analysis_data
)
summary(model_mixed_effects)


#Example way to display the model 
modelsummary(
  model_mixed_effects,
  statistic = "conf.int",  # shows CI instead of SE
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik", # keep only custom gof if desired
  gof_map = c(
    "nobs" = "Num. obs.",
    "ngrps_personid" = "Num. groups: personid",
    "ngrps_deckid" = "Num. groups: deckid",
    "var_personid.(Intercept)" = "Var: personid (Intercept)",
    "var_deckid.(Intercept)" = "Var: deckid (Intercept)",
    "sigma2" = "Var: Residual"
  )
)

# ----- MIXED-EFFECTS:

#H1 Actor: trust
model_H1_mixed <- lmer(
  trust ~ actor + FRT_concern + AI_lit + age + gender + education + urban_rural + minority + ideology +
    (1 | prolific_id) + (1 | deck_id),
  data = analysis_data
)
summary(model_H1_mixed)


# Start with core predictors
model_H1_base <- lmer(
  trust ~ actor + FRT_concern + (1 | prolific_id),
  data = analysis_data
)

summary(model_H1_base)

#H2: Scenario: overall acceptance 
model_H2_mixed <- lmer(
  FRT_acceptance_composite ~ scenario + FRT_concern + AI_lit + age + gender + education + state + urban_rural + minority + ideology +
    (1 | prolific_id) + (1 | deck_id),
  data = analysis_data
)
summary(model_H2_mixed)

#H3




#Model 1:
model_1 <- lm(supp ~ actor * scenario + type + scale + age + gender + education + FRT_knowledge + ai_lit + base_opinion, data = df)



model <- lm(
  FRT_acceptance_composite ~ scenario + actor + type + scale + AI_lit + baseline_FRT_support + FRT_knowledge + age + gender + education + state + urban_rural + minority + ideology,
  data = analysis_data
)

#Model 1: Actor - Supp
model_actor_1 <- lm(
  supp ~ actor + FRT_knowledge + FRT_concern + AI_lit + age + gender + education + state + urban_rural + minority + ideology,
  data = analysis_data
)
summary(model_actor_1)

#Model 2: Actor - Trust
model_actor_2 <- lm(
  trust ~ actor + FRT_knowledge + FRT_concern + AI_lit + age + gender + education + state + urban_rural + minority + ideology,
  data = analysis_data
)
summary(model_actor_2)

#Model 3: Actor - Behav
model_actor_3 <- lm(
  behav ~ actor + FRT_knowledge + FRT_concern + AI_lit + age + gender + education + state + urban_rural + minority + ideology,
  data = analysis_data
)
summary(model_actor_3)

#Model 4: Actor - Overall
model_actor_4 <- lm(
  FRT_acceptance_composite ~ actor + FRT_knowledge + FRT_concern + AI_lit + age + gender + education + state + urban_rural + minority + ideology,
  data = analysis_data
)
summary(model_actor_4)
