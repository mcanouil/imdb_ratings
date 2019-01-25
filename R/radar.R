options(stringsAsFactors = FALSE)

library(tidyverse)
library(scales)
library(gganimate)


devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')
theme_set(theme_black(base_size = 16))

range_dates <- 2013:2018

movies_theatres <- devtools::source_url(
  url = 'https://github.com/mcanouil/IMDbRating/raw/master/data/movies_theatres.R'
)$value %>% 
  (function(.movies_theatres) {
    .movies_theatres  %>% 
      mutate(
        Year = as.character(Year),
        Month = factor(Month, levels = locale()$date_names$mon)
      ) %>% 
      bind_rows(
        .movies_theatres  %>% 
          mutate(
            Year = as.character(Year),
            Month = factor(Month, levels = locale()$date_names$mon)
          ) %>% 
          group_by(Month) %>% 
          summarise(
            Year = "ALL",
            Count = floor(mean(Count, na.rm = TRUE))
          )
      ) %>% 
      arrange(Year, Month) %>%
      mutate(
        YM = paste0(Year, sprintf("%02d", as.numeric(Month))) %>% factor() %>% as.integer(),
        Year = factor(x = Year, levels = c(range_dates, "ALL")),
        Month_int = as.integer(Month)
      )
  })(.)

p <- ggplot(data = movies_theatres, mapping = aes(group = Year)) +
  coord_polar(theta = "x") +
  # theme
  theme_black(base_size = 16) +
    theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(0.75)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = 8, hjust = 0.5)
  ) +
  scale_y_continuous(
    limits = c(0, 22.5),
    breaks = seq(0, 20, by = 5),
    minor_breaks = seq(0, 20, by = 2.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq_len(12),
    labels = locale()$date_names$mon,
    expand = c(0, 0)
  ) +
  scale_colour_manual(
    name = "Year",
    values = c("white", viridis_pal(direction = 1)(length(range_dates))) %>% 
      `names<-`(c("ALL", range_dates))
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("white", viridis_pal(direction = 1)(length(range_dates))) %>% 
      `names<-`(c("ALL", range_dates))
  ) +
  scale_size_manual(
    name = "Year",
    values = c(5, rep(3, length(range_dates))) %>% 
      `names<-`(c("ALL", range_dates))
  ) +
  geom_text( # layer 1
    data = tibble(
      Count = c(0, rep(seq(5, 20, by = 5), times = 3)),
      Month = c(1, rep(c(1, 5, 8), each = 4))
    ),
    mapping = aes(x = Month, y = Count, label = Count),
    colour = ggplot2::theme_get()$text$colour,
    size = 16 * 1/4,
    inherit.aes = FALSE
  ) +
  geom_vline( # layer 2
    mapping = aes(xintercept = Month_int),
    colour = "grey50"
  ) +
  geom_point( # layer 3
    mapping = aes(x = Month_int, y = Count, colour = Year, size = Year)
  ) +
  geom_path( # layer 4
    mapping = aes(x = Month_int, y = Count, colour = Year),
    size = 1.5
  ) +
  labs(
    title = "# Movies Seen in Theatres",
    caption = "© Mickaël 'Coeos' Canouil"
  )


animate(
  plot = p + 
    transition_reveal(id = Year, along = Month_int, range = c(1L, 12L), keep_last = TRUE) +
    shadow_wake(
      wake_length = 0.15,
      wrap = TRUE,
      exclude_layer = c(1:5, 7, 8),
      exclude_phase = NULL
    ), 
  width = 500, 
  height = 500, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gifski_renderer(
    file = "./images/Coeos_IMDb_test.gif"
  )
)
