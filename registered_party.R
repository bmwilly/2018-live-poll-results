source("setup.R")

df <- dat %>%
  filter(
    file_party == "Other",
    likely == "Already voted"
  ) %>% 
  select(response, Age = ager, Education = educ, Gender = gender, `Race / Ethnicity` = race_eth, State = state) %>%
  mutate(
    Education = factor(Education, levels = educ_lvls),
    `Race / Ethnicity` = factor(`Race / Ethnicity`, levels = race_lvls)
  )

p_demos <- plot_demos(df)
p_state <- plot_states(df)

p_demos
p_state
