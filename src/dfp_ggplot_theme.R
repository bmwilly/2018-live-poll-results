theme_dfp <- function() {
  get_os <- function() {
    sysinf <- Sys.info()
    if (!is.null(sysinf)) {
      os <- sysinf["sysname"]
      if (os == "Darwin") {
        os <- "osx"
      }
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os)) {
        os <- "osx"
      }
      if (grepl("linux-gnu", R.version$os)) {
        os <- "linux"
      }
    }
    tolower(os)
  }
  if (get_os() == "osx") {
    theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(
          hjust = 1, size = 9,
          margin = margin(t = 10),
          face = "italic"
        ),
        plot.title = element_text(
          hjust = 0, size = 18,
          margin = margin(b = 10),
          face = "bold", family = "FuturaBT-Heavy"
        ),
        plot.subtitle = element_text(
          hjust = 0,
          family = "Montserrat-Regular"
        ),
        axis.title.y = element_text(
          size = 10, hjust = 1,
          face = "italic", family = "Montserrat-Thin"
        ),
        axis.title.x = element_text(
          hjust = 1, size = 10, face = "italic", family = "Montserrat-Thin",
          margin = margin(t = 10)
        ), # , margin = margin(t = 10)
        legend.position = "bottom",
        legend.title = element_text(face = "bold", family = "Montserrat-Regular"),
        text = element_text(family = "Montserrat-Regular")
      )
  }
  else {
    theme_bw() +
      theme(
        panel.border = element_blank(),
        plot.caption = element_text(
          hjust = 1, size = 9,
          margin = margin(t = 10),
          face = "italic"
        ),
        plot.title = element_text(
          hjust = 0, size = 18,
          margin = margin(b = 10),
          face = "bold", family = "FuturaBT-Heavy"
        ),
        plot.subtitle = element_text(
          hjust = 0,
          family = "Montserrat-Regular"
        ),
        axis.title.y = element_text(
          size = 10, hjust = 1,
          face = "italic", family = "Montserrat-Thin"
        ),
        axis.title.x = element_text(
          hjust = 1, size = 10, face = "italic", family = "Montserrat-Thin",
          margin = margin(t = 10)
        ), # , margin = margin(t = 10)
        legend.position = "bottom",
        legend.title = element_text(face = "bold", family = "Montserrat-Regular"),
        text = element_text(family = "Montserrat-Regular")
      )
  }
}
