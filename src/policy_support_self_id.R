source("src/setup.R")

questions <- c(
  "ASSAULTW" = "Do you support or oppose a federal ban on the sale of assault-style guns and high-capacity magazines?",
  "SINGLEPAY" = "Do you support the creation of a national insurance program, in which every American would get insurance from a single government plan?",
  "TAXREFORM" = "Do you support or oppose the tax reform bill passed by Congress and signed by the president last year?",
  "REVERSERACIS" = "Has discrimination against whites become as big a problem as discrimination toward blacks and other minorities?",
  "IMMCRIME" = "Are illegal immigrants living in the United States more likely than American citizens to commit serious crimes?",
  "CONIMMIG" = "Do you support a bill that would reduce legal immigration and provide funds for a wall along the U.S.-Mexican border?"
)

for (i in 1:length(questions)) {
  print(questions[[i]])
  print(table(dat[[names(questions[i])]]))
}

plots <- list()
dfs <- list()


others <- c(
  "[DO NOT READ] Don't know/Refused", 
  "[DO NOT READ] Refused", 
  "Independent (No party)",
  "or as a member of another political party"
)

likely_lvls <- c(
  "Already voted",
  "Almost certain",
  "Very likely",
  "Somewhat likely",
  "Not very likely",
  "Not at all likely",
  "[DO NOT READ] Don't know/Refused"
)

dfc <- dat %>%
  mutate(partyid = ifelse(partyid %in% others, "Other", partyid)) %>%
  mutate(partyid = factor(partyid, levels = c("Other", "Republican", "Democrat"))) %>%
  mutate(likely = factor(likely, levels = likely_lvls))

table(dfc$likely)
table(dfc$partyid)

for (i in 1:length(questions)) {
  question <- questions[[i]]
  col <- names(questions[i])

  df <- dfc
  df$col <- df[[col]]
  df <- df %>%
    drop_na(col) %>%
    filter(col != "Don't know") %>%
    group_by(partyid, col) %>%
    tally() %>%
    mutate(p = n / sum(n)) %>%
    filter(col %in% c("Support", "support", "agree"))

  if ('agree' %in% df$col) {
    subtitle <- paste("% of party that agrees; n =", sum(df$n))
  } else {
    subtitle <- paste("% of party that supports measure; n =", sum(df$n))
  }

  g <- ggplot(df, aes(partyid, p)) +
    geom_col(width = 0.75) +
    coord_flip() +
    geom_text(aes(label = scales::percent(p)), nudge_y = 0.06, family = 'Montserrat-Regular', size = 2.8) +
    scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(0, 1)) +
    labs(title = str_wrap(question, width = 44), subtitle = subtitle, x = "", y = "") +
    theme_dfp() +
    theme(
      plot.title = element_text(hjust = 0, size = 10, margin = margin(b = 10), face = "bold", family = 'FuturaBT-Heavy'),
      plot.subtitle = element_text(hjust = 0, size = 8, family = 'Montserrat-Regular')
    )

  dfs[[question]] <- df
  plots[[question]] <- g
}

df <- dfc
df$col <- df[[col]]

df <- df %>%
  drop_na(col) %>%
  filter(col != "Don't know") %>%
  group_by(partyid, likely, col) %>%
  tally() %>%
  mutate(p = n / sum(n)) %>%
  filter(col %in% c("Support", "support", "agree"))

ggplot(df, aes(partyid, p)) +
  geom_col(width = 0.75) +
  coord_flip() +
  geom_text(aes(label = scales::percent(p)), nudge_y = 0.06, family = 'Montserrat-Regular', size = 2.8) +
  facet_wrap(~likely) +   
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(0, 1)) +
  labs(title = str_wrap(question, width = 44), subtitle = subtitle, x = "", y = "") +
  theme_dfp() +
  theme(
    plot.title = element_text(hjust = 0, size = 10, margin = margin(b = 10), face = "bold", family = 'FuturaBT-Heavy'),
    plot.subtitle = element_text(hjust = 0, size = 8, family = 'Montserrat-Regular')
  )

g <- plot_grid(plotlist = plots)
save_plot("plots/policy_support_self_id.png", g, base_aspect_ratio = 3.4)


df <- data.frame()
for (i in 1:length(dfs)) {
  dft <- dfs[[i]]
  dft$question <- questions[[i]]
  df <- bind_rows(df, dft)
}

df %>%
  select(question, partyid, n, p) %>%
  write_csv("processed/policy_support_self_id.csv")
