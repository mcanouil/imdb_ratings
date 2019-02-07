options(stringsAsFactors = FALSE)

library(tidyverse)
library(scales)
library(gganimate)


devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')
theme_set(theme_black(base_size = 16, base_family = "xkcd"))

base_family <- "xkcd"
movies_theatres <- source('./data/movies_theatres.R')$value

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
  tidyr::drop_na()

grid_data <- tibble(
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
  tidyr::unnest()

ggplot2::ggplot(data = data_radar, mapping = ggplot2::aes(x = Month_int, y = Count, group = Year)) +
  ggplot2::labs(
    title = "# Movies Seen in Theatres",
    caption = "© Mickaël 'Coeos' Canouil"
  ) +
  ggplot2::coord_polar(theta = "x") +
  theme_black(base_size = 16, base_family = base_family) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(if(base_family=="xkcd"){0.85}else{0.75})),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(), # panel.grid.minor.x
    panel.border = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(size = 10, hjust = 0.5)
  ) +
  ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expand_scale(mult = c(0, 0.15))) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(12),
    labels = readr::locale()$date_names$mon,
    expand = c(0, 0)
  ) +
  # ggplot2::scale_colour_manual(
  #   values = c(scales::viridis_pal(direction = 1)(n_distinct(data_radar[["Year"]])-1), "white")
  # ) +
  # ggplot2::scale_fill_manual(
  #   values = c(scales::viridis_pal(direction = 1)(n_distinct(data_radar[["Year"]])-1), "white")
  # ) +
  # ggplot2::scale_size_manual(
  #   values = c(rep(3, n_distinct(data_radar[["Year"]])-1), 5)
  # ) +
  ggplot2::geom_hline( # layer 1
    data = filter(grid_data, type=="minor"),
    mapping = ggplot2::aes(yintercept = yintercept),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2
  ) +
  ggplot2::geom_hline( # layer 2
    data = filter(grid_data, type=="major"),
    mapping = ggplot2::aes(yintercept = yintercept),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4
  ) +
  ggplot2::geom_vline( # layer 3
    xintercept = seq_len(13),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2
  ) +
  ggplot2::geom_text( # layer 4
    data = dplyr::tibble(
      y = c(0, rep(unique(filter(grid_data, type=="major")[["yintercept"]])[-1], times = 4)),
      x = as.integer(c(1, rep(c(1, 4, 7, 10), each = 4)))
    ),
    mapping = ggplot2::aes(x = x, y = y, label = y),
    colour = ggplot2::theme_get()$text$colour,
    size = 16 * 1/4,
    inherit.aes = FALSE,
    family = base_family
  ) +
  ggplot2::geom_hline( # layer 5
    data = data.frame(Count = seq(0, 20, 5)),
    mapping = ggplot2::aes(yintercept = Count),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4,
    alpha = 1
  ) +
  ggplot2::geom_hline( # layer 6
    data = data.frame(Count = seq(0, 25, 5)),
    mapping = ggplot2::aes(yintercept = Count-5),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4,
    alpha = 1
  ) +
  ggplot2::geom_hline( # layer 7
    data = data.frame(Count = seq(0, 30, 5)),
    mapping = ggplot2::aes(yintercept = Count-10),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4,
    alpha = 1
  ) +
  ggplot2::geom_hline( # layer 8
    data = data.frame(Count = seq(0, 35, 5)),
    mapping = ggplot2::aes(yintercept = Count-15),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4,
    alpha = 1
  ) +
  ggplot2::geom_point( # layer 9
    mapping = ggplot2::aes(colour = Year, size = Year),
    na.rm = TRUE
  ) +
  gganimate::transition_reveal(along = Count, range = c(0, 20), keep_last = TRUE) +
  gganimate::shadow_wake(
    wake_length = 0.15,
    wrap = FALSE,
    # exclude_layer = c(1, 2, 3, 4, 9), 
    exclude_phase = NULL
  ) + 
  ggplot2::facet_wrap(facets = vars(Year)) +
  ggplot2::theme(legend.position = 'none')

