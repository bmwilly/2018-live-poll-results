addline_format <- function(x, ...) {
  gsub('\\s', '\n', x)
}


prepare_data <- function(df, col) {
  df$col <- df[[col]]
  
  dfc <- df %>%
    filter(!(col %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused"))) %>%
    filter(response %in% c("Dem", "Rep"))
  
  df1 <- dfc %>%
    group_by(col, response) %>%
    tally() %>%
    mutate(p = n / sum(n)) %>%
    select(-n) %>%
    spread(response, p, fill = 0) %>%
    mutate(diff = Dem - Rep)
  
  df2 <- dfc %>%
    group_by(col) %>%
    tally() %>%
    mutate(p = n / sum(n))
  
  dfp <- inner_join(df1, df2)
  
  df$col <- NULL
  
  dfp
}


plot_demo <- function(df, col) {
  dfp <- prepare_data(df, col)
  
  g <- dfp %>%
    ggplot(aes(fct_rev(col), p, fill = diff)) +
    geom_col(width = 0.75) + 
    geom_text(aes(label = scales::percent(p)), nudge_y = nudge_y[[col]], family = 'Montserrat-Regular', size = 2.8) +
    # geom_text(aes(label = n), nudge_y = nudge_y[[col]], family='Montserrat-Regular') + 
    coord_flip() +
    scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
    scale_fill_gradient2(low = colors['red'], high = colors['blue'], mid = colors['light_gray']) + 
    labs(title = "", x = "", y = "", fill = "") + 
    theme_dfp() +
    theme(legend.key.width = unit(1.5, "cm")) +
    theme()
  
  if (col == "Education") {
    g <- g + 
      scale_x_discrete(labels = addline_format(rev(levels(df$Education))[3:length(levels(df$Education))]))
  }
  
  g
}


plot_demos <- function(df) {
  plots <- c()
  
  demos <- df %>%
    select(-response, -State) %>%
    names()
  
  for (col in demos) {
    plots[[col]] <- plot_demo(df, col)
  }
  
  plot_grid(plotlist = plots)
}


plot_states <- function(df) {
  dfs <- prepare_data(df, 'State') %>%
    rename(State = col) %>%
    mutate(region = plyr::mapvalues(toupper(State), state.abb, tolower(state.name), warn_missing = FALSE))
  
  states <- map_data("state") %>%
    left_join(dfs)
  
  ggplot(states, aes(x = long, y = lat, group = group, fill = n), color = 'white') + 
    geom_polygon() + 
    coord_fixed(1.3) + 
    scale_fill_gradient(na.value = "white", low = colors['light_gray'], high = colors['dark_blue']) + 
    labs(title = "", x = "", y = "", fill = "") + 
    theme_dfp() + 
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.key.width = unit(3, "cm"))
}
