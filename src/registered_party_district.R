source("src/setup.R")

df <- dat %>%
  filter(
    file_party == "Other",
    likely == "Already voted"
  ) %>%
  select(response, District = district)

col <- "District"
df$col <- df[[col]]

dfc <- df %>%
  filter(!(col %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused"))) %>%
  filter(response %in% c("Dem", "Rep"))

dfts <- data.frame()
for (i in 1:5000) {
  dftt <- dfc %>%
    sample_frac(replace = TRUE) %>%
    group_by(col, response) %>%
    tally() %>%
    mutate(p = n / sum(n)) %>%
    select(-n) %>%
    spread(response, p, fill = 0) %>%
    mutate(diff = Dem - Rep) %>%
    mutate(shuffle = i)

  dfts <- bind_rows(dfts, dftt)
}

dfdt <- dfts %>%
  group_by(col) %>%
  summarize(sd = sd(diff)) %>%
  select(district = col, sd)

dfd <- prepare_data(df, 'District') %>%
  select(district = col, diff, n, p) %>%
  inner_join(dfdt)

ggplot(dfd, aes(fct_reorder(district, diff), diff)) +
  geom_point(aes(size = n, color = diff)) +
  geom_errorbar(aes(ymin = diff - sd, ymax = diff + sd), size = 0.1) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(-max(dfd$diff), max(dfd$diff))) +
  scale_color_gradient2(low = colors['red'], high = colors['blue'], mid = colors['light_gray']) +
  labs(title = "Party spread", x = "", y = "", size = "Sample size") +
  guides(color = FALSE) +
  theme_dfp()


dfd %>% write_csv("processed/district_spread.csv")
