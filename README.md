# 2018-live-poll-results

Data from the 2018 [New York Times Upshot/Siena College live polls](https://www.nytimes.com/interactive/2018/upshot/elections-polls.html)


## Setup

## Packages

In R,

```
install.packages("pacman")

pacman::p_load(
  dplyr,
  stringr,
  readr,
  tidyr,
  forcats,
  ggmap,
  cowplot,
  extrafont
)
```

### Fonts

Download the fonts [Montserrat](https://github.com/JulietaUla/Montserrat/releases) and
[FuturaBT Heavy](https://ufonts.com/download/futurabt-heavy-opentype.html) and add them
to your system's font book.

In R,

```
extrafont::font_import()
```