options(stringsAsFactors = FALSE)

library(tidyverse)
library(scales)
library(gganimate)


devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')
# theme_set(theme_black(base_size = 16, base_family = "xkcd"))

movies_theatres <- source('./data/movies_theatres.R')$value

range_dates <- min(movies_theatres[["Year"]]):max(movies_theatres[["Year"]]) # 2013:2018
p <- movies_theatres %>% 
  dplyr::mutate(Month = purrr::map_if(.x = Month, .p = ~.x=="January", .f = ~c(.x, "JD"))) %>% 
  tidyr::unnest() %>% 
  (function(.movies_theatres) {
    .movies_theatres  %>% 
      dplyr::mutate(
        Year = as.character(Year),
        Month = factor(Month, levels = c(readr::locale()$date_names$mon, "JD"))
      ) %>% 
      dplyr::bind_rows(
        .movies_theatres  %>% 
          dplyr::mutate(
            Year = as.character(Year),
            Month = factor(Month, levels = c(readr::locale()$date_names$mon, "JD"))
          ) %>% 
          dplyr::group_by(Month) %>% 
          dplyr::summarise(
            Year = "ALL",
            Count = floor(mean(Count, na.rm = TRUE))
          )
      ) %>% 
      dplyr::arrange(Year, Month) %>%
      dplyr::mutate(
        YM = paste0(Year, sprintf("%02d", as.numeric(Month))) %>% factor() %>% as.integer(),
        Year = factor(x = Year, levels = c(range_dates, "ALL")),
        Month_int = as.integer(Month)
      )
  })(.) %>% 
  ggplot2::ggplot(mapping = ggplot2::aes(group = Year)) +
  ggplot2::coord_polar(theta = "x") +
  # theme
  theme_black(base_size = 16, base_family = "xkcd") +
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
  ggplot2::scale_y_continuous(
    limits = c(0, 22.5),
    breaks = seq(0, 20, by = 5),
    minor_breaks = seq(0, 20, by = 2.5),
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(12),
    labels = readr::locale()$date_names$mon,
    expand = c(0, 0)
  ) +
  ggplot2::scale_colour_manual(
    name = "Year",
    values = c("white", viridis_pal(direction = 1)(length(range_dates))) %>% 
      `names<-`(c("ALL", range_dates))
  ) +
  ggplot2::scale_fill_manual(
    name = "Year",
    values = c("white", viridis_pal(direction = 1)(length(range_dates))) %>% 
      `names<-`(c("ALL", range_dates))
  ) +
  ggplot2::scale_size_manual(
    name = "Year",
    values = c(5, rep(3, length(range_dates))) %>% 
      `names<-`(c("ALL", range_dates))
  ) +
  ggplot2::geom_hline( # layer 1
    yintercept = seq(0, 20, by = 2.5),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2
  ) +
  ggplot2::geom_hline( # layer 2
    yintercept = seq(0, 20, by = 5),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4
  ) +
  ggplot2::geom_hline( # layer 3
    yintercept = 20,
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4
  ) +
  ggplot2::geom_vline( # layer 4
    xintercept = seq_len(12),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2
  ) +
  ggplot2::geom_text( # layer 5
    data = dplyr::tibble(
      Count = c(0, rep(seq(5, 20, by = 5), times = 4)), 
      Month = c(1, rep(c(1, 4, 7, 10), each = 4))
    ),
    mapping = ggplot2::aes(x = Month, y = Count, label = Count),
    colour = ggplot2::theme_get()$text$colour,
    size = 16 * 1/4,
    inherit.aes = FALSE,
    family = "xkcd"
  ) +
  ggplot2::geom_vline( # layer 6
    mapping = ggplot2::aes(xintercept = Month_int),
    colour = "grey50"
  ) +
  ggplot2::geom_point( # layer 7
    mapping = ggplot2::aes(x = Month_int, y = Count, colour = Year, size = Year)
  ) +
  ggplot2::geom_path( # layer 8
    mapping = ggplot2::aes(x = Month_int, y = Count, colour = Year),
    size = 1.5
  ) +
  ggplot2::labs(
    title = "# Movies Seen in Theatres",
    caption = "© Mickaël 'Coeos' Canouil"
  )


p_animate2 <- p + 
  gganimate::transition_reveal(along = Month_int, range = c(1L, 13L), keep_last = TRUE) +
  gganimate::shadow_wake(
    wake_length = 0.15,
    wrap = TRUE,
    exclude_layer = c(1:5, 7, 8),
    exclude_phase = NULL
  )

gganimate::animate(
  plot = p_animate2, 
  width = 500, 
  height = 500, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(
    file = "./images/Coeos_IMDb_radar.gif"
  )
)
