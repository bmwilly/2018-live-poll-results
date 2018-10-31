library(tidyverse)
library(ggmap)
library(cowplot)

source("dfp_ggplot_theme.R")

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
  "Black",
  "Hispanic",
  "Asian",
  "Other",
  "[DO NOT READ] Refused",
  "[DO NOT READ] Don't know/Refused"
)

df <- dat %>%
  filter(partyid == "Independent (No party)") %>%  # should "or as a member of another political party" be included? 
  select(response, Age = ager, Education = educ, Gender = gender, `Race / Ethnicity` = race_eth, State = state) %>%
  mutate(
    Education = factor(Education, levels = educ_lvls),
    `Race / Ethnicity` = factor(`Race / Ethnicity`, levels = race_lvls)
  )

plot_demo <- function(col) {
  df$col <- df[[col]]
  dfp <- df %>%
    filter(!(col %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused"))) %>%
    group_by(col, response) %>%
    tally() %>%
    filter(response %in% c("Dem", "Rep")) %>%
    mutate(p = n / sum(n)) %>%
    select(-n) %>%
    spread(response, p) %>%
    mutate(diff = Dem - Rep)
  df$col <- NULL
  
  dfp %>%
    ggplot(aes(fct_rev(col), diff, fill = diff)) +
    geom_col(width = 0.8) + 
    coord_flip() +
    scale_fill_gradient2(low = colors['red'], high = colors['blue']) + 
    labs(title = paste("Early voting from Independents by", col), x = "", y = "", fill = "") + 
    theme_dfp() +
    theme(legend.key.width = unit(2.5, "cm"))
}

plots <- c()

demos <- df %>%
  select(-response, -State) %>%
  names()

for (col in demos) {
  plots[[col]] <- plot_demo(col)
}

plots[['Age']]
plots[['Education']]
plots[['Gender']]
plots[['Race / Ethnicity']]

save_plot("plots/age.png", plots[['Age']])
save_plot("plots/education.png", plots[['Education']])
save_plot("plots/gender.png", plots[['Gender']])
save_plot("plots/race_eth.png", plots[['Race / Ethnicity']])


## States

dfs <- df %>%
  filter(!(State %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused"))) %>%
  group_by(State, response) %>%
  tally() %>%
  filter(response %in% c("Dem", "Rep")) %>%
  mutate(p = n / sum(n)) %>%
  select(-n) %>%
  spread(response, p) %>%
  mutate(diff = Dem - Rep) %>%
  mutate(region = plyr::mapvalues(toupper(State), state.abb, tolower(state.name), warn_missing = FALSE))

states <- map_data("state") %>%
  left_join(dfs)

plots[['State']] <- ggplot(states, aes(x = long, y = lat, group = group, fill = diff), color = 'white') + 
  geom_polygon() + 
  coord_fixed(1.3) + 
  scale_fill_gradient2(low = colors['red'], high = colors['blue'], na.value = "white") + 
  labs(title = "Early voting by state", x = "", y = "", fill = "") + 
  theme_dfp() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.key.width = unit(3, "cm"))

plots[['State']]

save_plot("plots/state.png", plots[['State']])
