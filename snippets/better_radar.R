options(stringsAsFactors = FALSE)

library(tidyverse)
library(scales)
library(gganimate)


devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')
theme_set(theme_black(base_size = 16, base_family = "xkcd"))

base_family <- ""
movies_theatres <- devtools::source_url('https://github.com/mcanouil/IMDbRating/raw/master/data/movies_theatres.R')$value

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

# data_radar <- data_radar %>% 
#   dplyr::filter(Year%in%c("2018")) %>% 
#   dplyr::mutate(Year = factor(Year))

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

radar_base <- ggplot2::ggplot(data = data_radar, mapping = ggplot2::aes(x = Month_int, y = Count, group = Year)) +
  ggplot2::labs(
    title = "# Movies Seen in Theatres",
    caption = "© Mickaël 'Coeos' Canouil"
  ) +
  ggplot2::coord_polar(theta = "x") +
  theme_black(base_size = 16, base_family = base_family) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(if (base_family=="xkcd") {0.85} else {0.75})),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(), # panel.grid.minor.x
    panel.border = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(size = 10, hjust = 0.5)
  ) +
  ggplot2::scale_y_continuous(
    limits = range(grid_data[["yintercept"]]), 
    expand = ggplot2::expand_scale(mult = c(0, 0.15))
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(12),
    labels = readr::locale()$date_names$mon,
    expand = c(0, 0)
  ) +
  ggplot2::scale_colour_viridis_d(begin = 0.4) +
  ggplot2::scale_fill_viridis_d(begin = 0.4) +
  ggplot2::geom_hline( # layer 1
    data = dplyr::filter(grid_data, type=="minor"),
    mapping = ggplot2::aes(yintercept = yintercept),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2,
    na.rm = TRUE
  ) +
  ggplot2::geom_hline( # layer 2
    data = dplyr::filter(grid_data, type=="major"),
    mapping = ggplot2::aes(yintercept = yintercept),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4,
    na.rm = TRUE
  ) +
  ggplot2::geom_vline( # layer 3
    xintercept = seq_len(13),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2
  ) +
  ggplot2::geom_text( # layer 4
    data = dplyr::tibble(
      y = c(0, rep(unique(dplyr::filter(grid_data, type=="major")[["yintercept"]])[-1], times = 4)),
      x = as.integer(c(1, rep(c(1, 4, 7, 10), each = 4)))
    ),
    mapping = ggplot2::aes(x = x, y = y, label = y),
    colour = ggplot2::theme_get()$text$colour,
    size = 16 * 1/4,
    inherit.aes = FALSE,
    family = base_family
  )


### Radar: Point
gganimate::animate(
  plot = radar_base +
    ggplot2::geom_vline( # layer 5
      data = data.frame(Month_int = seq_len(13)),
      mapping = ggplot2::aes(xintercept = Month_int),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.4,
      na.rm = TRUE
    ) +
    ggplot2::geom_point( # layer 6
      mapping = ggplot2::aes(fill = Year, group = paste(Year, Month_int)),
      na.rm = TRUE,
      shape = 21,
      size = 5,
      colour = ggplot2::theme_get()$text$colour
    ) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::facet_wrap(facets = ggplot2::vars(Year), nrow = 2) +
    gganimate::transition_reveal(along = Month_int, range = c(1L, 13L), keep_last = TRUE) +
    gganimate::shadow_wake(
      wake_length = 1 / 3,
      wrap = TRUE
    ), 
  width = 500 * 2.5, 
  height = 500 * 1.5, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(
    file = "./images/radar_test0.gif"
  )
)
  
  
### Waves: Point
gganimate::animate(
  plot = radar_base +
    purrr::map(.x = seq(from = 0, to = 3, by = 1.5), .f = function(.x) { # layer 5-8
      tmp_data <- grid_data %>% 
        dplyr::filter(type=="major")
      waves_interval <- .x * unique(diff(tmp_data[["yintercept"]]))
      ggplot2::geom_hline(
        data = tmp_data %>% 
          dplyr::mutate(Count = yintercept + waves_interval),
        mapping = ggplot2::aes(yintercept = Count - waves_interval),
        colour = ggplot2::theme_get()$panel.grid$colour,
        size = 0.4,
        na.rm = TRUE
      )
    }) +
    ggplot2::geom_point( # layer 9
      mapping = ggplot2::aes(fill = Year, group = paste(Year, Month_int)),
      na.rm = TRUE,
      shape = 21,
      size = 5,
      colour = ggplot2::theme_get()$text$colour
    ) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::facet_wrap(facets = ggplot2::vars(Year), nrow = 2) +
    gganimate::transition_reveal(along = Count, range = c(0, max(grid_data[["yintercept"]])*1.25), keep_last = TRUE) +
    gganimate::shadow_wake(
      wake_length = 1 / 3,
      wrap = TRUE
    ), 
  width = 500 * 2.5, 
  height = 500 * 1.5, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(
    file = "./images/radar_test1.gif"
  )
)


### Waves: Path + Point
gganimate::animate(
  plot = radar_base +
    purrr::map(.x = seq(from = 0, to = 3, by = 1.5), .f = function(.x) { # layer 5-8
      tmp_data <- grid_data %>% 
        dplyr::filter(type=="major")
      waves_interval <- .x * unique(diff(tmp_data[["yintercept"]]))
      ggplot2::geom_hline(
        data = tmp_data %>% 
          dplyr::mutate(Count = yintercept + waves_interval),
        mapping = ggplot2::aes(yintercept = Count - waves_interval),
        colour = ggplot2::theme_get()$panel.grid$colour,
        size = 0.4,
        na.rm = TRUE
      )
    }) +
    ggplot2::geom_path( # layer 9
      mapping = ggplot2::aes(colour = Year),
      na.rm = TRUE,
      size = 1.5
    ) +
    ggplot2::geom_point( # layer 10
      mapping = ggplot2::aes(colour = Year, group = paste(Year, Month_int)),
      na.rm = TRUE,
      size = 5
    ) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::facet_wrap(facets = ggplot2::vars(Year), nrow = 2) +
    gganimate::transition_reveal(along = Count, range = c(0, max(grid_data[["yintercept"]])*1.25), keep_last = TRUE) +
    gganimate::shadow_wake(
      wake_length = 1 / 3,
      wrap = TRUE
    ), 
  width = 500 * 2.5, 
  height = 500 * 1.5, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(
    file = "./images/radar_test2.gif"
  )
)


###
gganimate::animate(
  plot = radar_base +
    ggplot2::geom_path( # layer 5
      mapping = ggplot2::aes(colour = Year),
      size = 1.5,
      na.rm = TRUE
    ) +
    ggplot2::geom_point( # layer 6
      mapping = ggplot2::aes(colour = Year, fill = Year), 
      shape = 21,
      colour = ggplot2::theme_get()$text$colour,
      na.rm = TRUE
    ) +
    ggplot2::geom_smooth( # layer 7
      mapping = ggplot2::aes(x = Month_int, y = Count),
      colour = ggplot2::theme_get()$text$colour,
      method = "gam",
      se = FALSE,
      size = 1.5,
      linetype = 2,
      show.legend = FALSE
    ) +
    gganimate::transition_states(
      Year,
      transition_length = 5,
      state_length = 25
    ) +
    gganimate::enter_appear(early = FALSE) +
    gganimate::exit_disappear(early = FALSE) +
    gganimate::ease_aes('linear') +
    ggplot2::labs(subtitle = "{closest_state}") +
    ggplot2::theme(legend.position = "none"), 
  width = 500, 
  height = 500, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(
    file = "./images/radar_test3.gif"
  )
)


###
gganimate::animate(
  plot = radar_base +
    ggplot2::geom_vline( # layer 5
      data = data.frame(Month_int = seq_len(13)),
      mapping = ggplot2::aes(xintercept = Month_int),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.4,
      na.rm = TRUE
    ) +
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
    gganimate::transition_reveal(along = Month_int, range = c(1L, 13L), keep_last = TRUE) +
    gganimate::shadow_wake(
      wake_length = 1 / 3,
      wrap = TRUE
    ), 
  width = 500, 
  height = 500, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(
    file = "./images/radar_test4.gif"
  )
)
