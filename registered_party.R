source("setup.R")

df <- dat %>%
  filter(
    file_party == "Other",
    likely == "Already voted"
  ) %>% 
  select(response, Age = ager, Education = educ, Gender = gender, `Race / Ethnicity` = race_eth, State = state, District = district) %>%
  mutate(
    Education = factor(Education, levels = educ_lvls),
    `Race / Ethnicity` = factor(`Race / Ethnicity`, levels = race_lvls)
  )

dfd <- prepare_data(df, 'District') %>%
  select(district = col, diff, n, p)

ggplot(dfd, aes(fct_reorder(district, diff), diff)) + 
  geom_point(aes(size = n, color = diff)) + 
  geom_hline(yintercept = 0) + 
  coord_flip() + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(-max(dfd$diff), max(dfd$diff))) + 
  scale_color_gradient2(low = colors['red'], high = colors['blue'], mid = colors['light_gray']) + 
  labs(title = "Party spread", x = "", y = "", size = "Sample size") +
  guides(color = FALSE) + 
  theme_dfp()

dfd %>% write_csv("output/district_spread.csv")

p_demos <- plot_demos(df)
p_state <- plot_states(df)

p_demos
p_state
