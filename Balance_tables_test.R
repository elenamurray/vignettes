#Comparison tables - code
#Nevermind I think it is working!!

#-------------
#Compare the balance of vignette attributes for d-efficient and random sampling

#Balance check - d-efficient
d_sample <- design_matrix %>%
  filter(deck_id %in% deck_combinations_defficient$deck_id)

#Pivot
d_long <- d_sample %>%
  pivot_longer(
    cols = matches("_(actor|type|scale)$"),
    names_to = c("scenario", "attribute"),
    names_pattern = "(park|shopping|stadium)_(actor|type|scale)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = attribute,
    values_from = value
  ) %>%
  mutate(method = "D-efficient")

#Balance check - random sampling
r_sample <- design_matrix %>%
  filter(deck_id %in% deck_combinations_random_sampling$deck_id)

#Pivot
r_long <- r_sample %>%
  pivot_longer(
    cols = matches("_(actor|type|scale)$"),
    names_to = c("scenario", "attribute"),
    names_pattern = "(park|shopping|stadium)_(actor|type|scale)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = attribute,
    values_from = value
  ) %>%
  mutate(method = "Random")

#Bind and summarize for comparison
combined <- bind_rows(d_long, r_long)

summary <- combined %>%
  pivot_longer(cols = c(actor, type, scale), names_to = "attribute", values_to = "value") %>%
  group_by(method, attribute, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(method, attribute) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         summary = paste0(n, " (", percent, "%)")) %>%
  ungroup()

#Pivot for side-by-side comparison
comparison_table <- summary %>%
  select(method, attribute, value, summary) %>%
  pivot_wider(names_from = method, values_from = summary)

#Display with gt
comparison_table %>%
  gt() %>%
  tab_header(
    title = md("**Attribute Distribution: D-efficient vs Random Sample**")
  ) %>%
  cols_label(
    attribute = "Attribute",
    value = "Value",
    `D-efficient` = "D-efficient (n, %)",
    Random = "Random (n, %)"
  )


#-------------
#Compare the balance of vignette attributes for d-efficient and random sampling - WITH CONSTRAINTS

#Balance check - d-efficient
d_sample_constrained <- design_matrix %>%
  filter(deck_id %in% deck_combinations_constrained_deff_100$deck_id)

#Pivot
d_long_constrained <- d_sample_constrained %>%
  pivot_longer(
    cols = matches("_(actor|type|scale)$"),
    names_to = c("scenario", "attribute"),
    names_pattern = "(park|shopping|stadium)_(actor|type|scale)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = attribute,
    values_from = value
  ) %>%
  mutate(method = "D-efficient")

#Balance check - random sampling
r_sample_constrained <- design_matrix %>%
  filter(deck_id %in% deck_combinations_constrained_random$deck_id)

#Pivot
r_long_constrained <- r_sample_constrained %>%
  pivot_longer(
    cols = matches("_(actor|type|scale)$"),
    names_to = c("scenario", "attribute"),
    names_pattern = "(park|shopping|stadium)_(actor|type|scale)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = attribute,
    values_from = value
  ) %>%
  mutate(method = "Random")

#Bind and summarize for comparison
combined_constrained <- bind_rows(d_long_constrained, r_long_constrained)

summary_constrained <- combined_constrained %>%
  pivot_longer(cols = c(actor, type, scale), names_to = "attribute", values_to = "value") %>%
  group_by(method, attribute, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(method, attribute) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         summary = paste0(n, " (", percent, "%)")) %>%
  ungroup()

#Pivot for side-by-side comparison
comparison_table_constrained <- summary_constrained %>%
  select(method, attribute, value, summary) %>%
  pivot_wider(names_from = method, values_from = summary)

#Display with gt
comparison_table_constrained %>%
  gt() %>%
  tab_header(
    title = md("**Attribute Distribution (Constrained): D-efficient vs Random Sample**")
  ) %>%
  cols_label(
    attribute = "Attribute",
    value = "Value",
    `D-efficient` = "D-efficient (n, %)",
    Random = "Random (n, %)"
  )