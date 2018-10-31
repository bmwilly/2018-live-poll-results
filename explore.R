library(tidyverse)
library(ggmap)
library(cowplot)

source("dfp_ggplot_theme.R")

extrafont::font_import()

colors <- c(
  "blue" = "#124073",
  "dark_blue" = "#0A2645",
  "green" = "#A8BF14",
  "red" = "#B71D1A",
  "orange" = "#BF7800",
  "light_gray" = "#B3B3B3"
)

fnames <- list.files(path = "data")
tmp <- lapply(str_split(fnames, "elections-poll-"), function(x) x[[2]])
states <- lapply(tmp, function(x) substr(x, 1, 2))

dat <- lapply(list.files(path = "data", full.names = TRUE), read_csv)
for (i in 1:length(states)) {
  dat[[i]]$state <- states[[i]]
}

dat <- bind_rows(dat)

educ_lvls <- c(
  "Grade school", 
  "High school", 
  "Some college or trade school", 
  "Bachelors' degree",
  "Graduate or Professional Degree",
  "[DO NOT READ] Refused",
  "[DO NOT READ] Don't know/Refused"
)

race_lvls <- c(
  "White",
  "Hispanic",
  "Black",
  "Asian",
  "Other",
  "[DO NOT READ] Refused",
  "[DO NOT READ] Don't know/Refused"
)

df <- dat %>%
  filter(
    partyid == "Independent (No party)",  # should "or as a member of another political party" be included? 
    likely == "Already voted"
  ) %>% 
  select(response, Age = ager, Education = educ, Gender = gender, `Race / Ethnicity` = race_eth, State = state) %>%
  mutate(
    Education = factor(Education, levels = educ_lvls),
    `Race / Ethnicity` = factor(`Race / Ethnicity`, levels = race_lvls)
  )

addline_format <- function(x, ...) {
  gsub('\\s', '\n', x)
}

nudge_y <- c(
  "Age" = 0.02,
  "Education" = 0.018,
  "Gender" = 0.035,
  "Race / Ethnicity" = 0.04
)

prepare_data <- function(col) {
  df$col <- df[[col]]
  
  dfc <- df %>%
    filter(!(col %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused")))
  
  df1 <- dfc %>%
    group_by(col, response) %>%
    tally() %>%
    filter(response %in% c("Dem", "Rep")) %>%
    mutate(p = n / sum(n)) %>%
    select(-n) %>%
    spread(response, p) %>%
    mutate(diff = Dem - Rep)
  
  df2 <- dfc %>%
    group_by(col) %>%
    tally() %>%
    mutate(p = n / sum(n))
  
  dfp <- inner_join(df1, df2)
  
  df$col <- NULL
  
  dfp
}


plot_demo <- function(col) {
  dfp <- prepare_data(col)
  
  g <- dfp %>%
    ggplot(aes(fct_rev(col), p, fill = diff)) +
    geom_col(width = 0.8) + 
    geom_text(aes(label = scales::percent(p)), nudge_y = nudge_y[[col]], family='Montserrat-Regular') +
    # geom_text(aes(label = n), nudge_y = nudge_y[[col]], family='Montserrat-Regular') + 
    coord_flip() +
    scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
    scale_fill_gradient2(low = colors['red'], high = colors['blue'], mid = colors['light_gray']) + 
    labs(title = "", x = "", y = "", fill = "") + 
    theme_dfp() +
    theme(legend.key.width = unit(1.5, "cm"))
  
  if (col == "Education") {
    g <- g + 
      scale_x_discrete(labels = addline_format(rev(levels(df$Education))[3:length(levels(df$Education))]))
  }
  
  g
}

plots <- c()

demos <- df %>%
  select(-response, -State) %>%
  names()

for (col in demos) {
  plots[[col]] <- plot_demo(col)
}

g <- plot_grid(plotlist = plots)
g

plots[['Age']]
plots[['Education']]
plots[['Gender']]
plots[['Race / Ethnicity']]

save_plot("plots/age.png", plots[['Age']])
save_plot("plots/education.png", plots[['Education']])
save_plot("plots/gender.png", plots[['Gender']])
save_plot("plots/race_eth.png", plots[['Race / Ethnicity']])


## States

dfs <- prepare_data('State') %>%
  rename(State = col) %>%
  mutate(region = plyr::mapvalues(toupper(State), state.abb, tolower(state.name), warn_missing = FALSE))

states <- map_data("state") %>%
  left_join(dfs)

plots[['State']] <- ggplot(states, aes(x = long, y = lat, group = group, fill = n), color = 'white') + 
  geom_polygon() + 
  coord_fixed(1.3) + 
  scale_fill_gradient(na.value = "white", low = colors['light_gray'], high = colors['dark_blue']) + 
  labs(title = "", x = "", y = "", fill = "") + 
  theme_dfp() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.key.width = unit(3, "cm"))

plots[['State']]

save_plot("plots/state.png", plots[['State']])
