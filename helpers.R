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