library(tidyverse)
library(ggmap)
library(cowplot)

source("dfp_ggplot_theme.R")
source("helpers.R")


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

table(dat$file_party)

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

addline_format <- function(x, ...) {
  gsub('\\s', '\n', x)
}

nudge_y <- c(
  "Age" = 0.02,
  "Education" = 0.018,
  "Gender" = 0.035,
  "Race / Ethnicity" = 0.04
)



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

head(df)

table(dfc$response)

df %>%
  filter(response %in% c("Dem", "Rep")) %>%
  pull(response) %>%
  table()
