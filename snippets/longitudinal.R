options(gganimate.dev_args = list(width = 672, height = 480))
### Load packages
library(tidyverse)
library(viridis)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(rvest)
library(gganimate)

devtools::source_url("https://github.com/mcanouil/DEV/raw/master/R/theme_black.R")
devtools::source_url("https://github.com/mcanouil/DEV/raw/master/R/format.R")

base_family <- if ("sysfonts" %in% installed.packages()) "xkcd" else ""

### functions

theme_set(theme_black(base_size = 9, base_family = base_family))

all_movies_theatres <- source("./data/movies_theatres.R")$value
movies_theatres <- all_movies_theatres %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(Count = n)

data_radar <- movies_theatres %>% 
  dplyr::mutate(Month = purrr::map_if(.x = Month, .p = ~.x=="January", .f = ~c(.x, "JD"))) %>% 
  tidyr::unnest() %>% 
  (function(.x) {
    dplyr::bind_rows(
    .x %>% 
      dplyr::mutate(Year = as.character(Year)),
    .x %>% 
      dplyr::group_by(Month) %>% 
      dplyr::summarise(
        Year = "ALL",
        Count = floor(mean(Count, na.rm = TRUE))
      )
    )
  })(.) %>% 
  dplyr::mutate(
    Year = factor(x = Year, levels = c(setdiff(sort(unique(Year)), "ALL"), "ALL")),
    Month = factor(Month, levels = c(readr::locale()$date_names$mon, "JD")),
    Month_int = as.integer(Month)
  ) %>% 
  dplyr::arrange(Year, Month) %>% 
  tidyr::drop_na() %>% 
  filter(Month!="JD") %>% 
  filter(!Year %in% c("2013", "ALL"))

grid_data <- dplyr::tibble(
  major = scales::pretty_breaks(5)(c(0, max(data_radar[["Count"]], na.rm = TRUE)))
) %>% 
  dplyr::mutate(
    minor = c(major[-length(major)] + diff(major)/2, NA)
  ) %>% 
  tidyr::gather(key = "type", value = "yintercept") %>% 
  dplyr::mutate(
    size = c("major" = 0.4, "minor" = 0.2)[type],
    Year = list(unique(data_radar[["Year"]]))
  ) %>% 
  tidyr::drop_na() %>% 
  tidyr::unnest() %>% 
  dplyr::select(-Year) %>% 
  dplyr::distinct()

ggplot2::ggplot(data = data_radar, mapping = ggplot2::aes(x = Month_int, y = Count, group = Year)) +
  ggplot2::labs(
    title = "# Movies Seen in Theatres",
    caption = "© Mickaël 'Coeos' Canouil"
  ) +
  # ggplot2::coord_polar(theta = "x") +
  theme_black(base_size = 16, base_family = base_family) +
  ggplot2::theme(
    # axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(), # panel.grid.minor.x
    panel.border = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(size = 10, hjust = 0.5),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  # ggplot2::scale_y_continuous(
  #   expand = ggplot2::expand_scale(mult = c(0.15, 0.15))
  # ) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(12),
    labels = readr::locale()$date_names$mon
  ) +
  ggplot2::scale_colour_viridis_d(begin = 0.4) +
  ggplot2::scale_fill_viridis_d(begin = 0.4) +
  ggplot2::geom_path( # layer 6
    mapping = ggplot2::aes(colour = Year),
    size = 1.5,
    na.rm = TRUE
  ) +
  ggplot2::geom_point( # layer 7
    mapping = ggplot2::aes(fill = Year, group = paste(Year, Month_int)),
    na.rm = TRUE,
    shape = 21,
    size = 5,
    colour = ggplot2::theme_get()$text$colour
  ) +
  gganimate::transition_reveal(
    along = Month_int, 
    range = c(1L, 13L), 
    keep_last = TRUE
  ) +
  gganimate::shadow_wake(
    wake_length = 1 / 3,
    wrap = TRUE
  )