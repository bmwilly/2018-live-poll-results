## Packages

pacman::p_load(
  dplyr,
  stringr,
  readr,
  tidyr,
  forcats,
  ggmap,
  cowplot
)

## Functions

source("dfp_ggplot_theme.R")
source("helpers.R")

## Constants

colors <- c(
  "blue" = "#124073",
  "dark_blue" = "#0A2645",
  "green" = "#A8BF14",
  "red" = "#B71D1A",
  "orange" = "#BF7800",
  "light_gray" = "#B3B3B3"
)

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

nudge_y <- c(
  "Age" = 0.02,
  "Education" = 0.018,
  "Gender" = 0.035,
  "Race / Ethnicity" = 0.04
)

## Data

fnames <- list.files(path = "../data")
tmp <- lapply(str_split(fnames, "elections-poll-"), function(x) x[[2]])
states <- lapply(tmp, function(x) substr(x, 1, 2))
districts <- lapply(tmp, function(x) substr(x, 1, 4))
polls <- lapply(tmp, function(x) substr(x, 1, 6))

dat <- lapply(list.files(path = "../data", full.names = TRUE), read_csv)
for (i in 1:length(states)) {
  dat[[i]]$state <- states[[i]]
  dat[[i]]$district <- districts[[i]]
  dat[[i]]$poll <- polls[[i]]
}

dat <- bind_rows(dat)

## Cleanup

rm(states, districts, polls, tmp, i)
