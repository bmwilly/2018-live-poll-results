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
  # "[DO NOT READ] Don't know/Refused", 
  # "[DO NOT READ] Refused", 
  "Independent (No party)"
  # "or as a member of another political party"
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
  mutate(partyid = ifelse(partyid %in% others, "Independent", partyid)) %>%
  mutate(partyid = factor(partyid, levels = c("Independent", "Republican", "Democrat"))) %>%
  mutate(likely = factor(likely, levels = likely_lvls)) %>%
  filter(!is.na(partyid))


## Overall policy support

for (i in 1:length(questions)) {
  
  if (i %in% c(1, 2)) {
    support_colors <- c(
      "Support" = colors[['blue']],
      "Oppose" = colors[['red']],
      "Don't Know" = colors[['light_gray']]
    )
  } else if (i %in% c(3, 6)) {
    support_colors <- c(
      "Support" = colors[['red']],
      "Oppose" = colors[['blue']],
      "Don't Know" = colors[['light_gray']]
    )
  } else {
    support_colors <- c(
      "Agree" = colors[['red']],
      "Disagree" = colors[['blue']],
      "Don't Know" = colors[['light_gray']]
    )
  }
  
  question <- questions[[i]]
  col <- names(questions[i])
  
  df <- dfc
  df$col <- df[[col]]
  df <- df %>%
    drop_na(col) %>%
    # filter(col != "Don't know") %>%
    group_by(col) %>%
    # tally() %>%
    summarize(n = n(), w = sum(w_LV)) %>%
    mutate(p = w / sum(w)) %>%
    mutate(question = question) %>%
    mutate(col = str_to_title(col)) %>%
    mutate(col = fct_rev(factor(col, levels = c("Support", "Oppose", "Agree", "Disagree", "Don't Know"))))
  
  g <- ggplot(df, aes(col, p, fill = col)) + 
    geom_col() + 
    coord_flip() +
    geom_text(aes(label = scales::percent(p)), nudge_y = 0.06, family = 'Montserrat-Regular', size = 2.8) +
    scale_fill_manual(values = support_colors) + 
    scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(0, 1)) +
    labs(title = str_wrap(question, width = 44), subtitle = paste("n =", sum(df$n)), x = "", y = "") +
    guides(fill = FALSE) + 
    theme_dfp() + 
    theme(
      plot.title = element_text(hjust = 0, size = 10, margin = margin(b = 10), face = "bold", family = 'FuturaBT-Heavy'),
      plot.subtitle = element_text(hjust = 0, size = 8, family = 'Montserrat-Regular')
    )
  
  dfs[[question]] <- df
  plots[[question]] <- g
}


g <- plot_grid(plotlist = plots, nrow = 2)
g
save_plot("plots/policy_support_overall.png", g, base_aspect_ratio = 3.4)
# save_plot("plots/policy_support_overall.png", g, base_height = 6, base_aspect_ratio = 1.5)


## Policy support by party

for (i in 1:length(questions)) {
  question <- questions[[i]]
  col <- names(questions[i])

  df <- dfc
  df$col <- df[[col]]
  df <- df %>%
    drop_na(col) %>%
    filter(col != "Don't know") %>%
    group_by(partyid, col) %>%
    # tally() %>%
    summarize(n = n(), w = sum(w_LV)) %>%
    mutate(p = w / sum(w)) 

  # if ('agree' %in% df$col) {
  #   subtitle <- paste("% of party that agrees; n =", sum(df$n))
  # } else {
  #   subtitle <- paste("% of party that supports measure; n =", sum(df$n))
  # }
  
  if (i %in% c(4, 5)) {
    subtitle <- "% of party in agreement"
  } else {
    subtitle <- "% of party that supports measure"
  }

  g <- df %>%
    filter(col %in% c("Support", "support", "agree")) %>%
    ggplot(aes(partyid, p)) +
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

g <- plot_grid(plotlist = plots, nrow = 2)
g
save_plot("plots/policy_support_self_id.png", g, base_aspect_ratio = 3.4)
# save_plot("plots/policy_support_self_id.png", g, base_height = 6, base_aspect_ratio = 1.5)


## Write data

df <- data.frame()
for (i in 1:length(dfs)) {
  dft <- dfs[[i]]
  dft$question <- questions[[i]]
  df <- bind_rows(df, dft)
}

df %>%
  select(question, partyid, n, p) %>%
  write_csv("processed/policy_support_self_id.csv")
