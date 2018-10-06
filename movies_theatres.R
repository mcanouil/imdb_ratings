# rm(list = ls())
options(stringsAsFactors = FALSE)

# working_directory <- "/media/data/IMDbRating"
# setwd(working_directory)
# ggsave(file = "Pictures/.png", plot = p, width = 6.3, height = 4.7, units = "in", dpi = 300)


### Load packages
library(tidyverse)
library(viridis)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(gganimate)

devtools::source_url('https://github.com/mcanouil/DEV/raw/master/Rfunctions/theme_black.R')


### Define theme
theme_set(theme_black(base_size = 14))


### data
month_factor <- factor(locale()$date_names$mon, levels = locale()$date_names$mon)
movies_theatres <- tribble(
  ~Year, ~Month, ~Count,
  2013, "January", NA,
  2013, "February", NA,
  2013, "March", NA,
  2013, "April", 8,
  2013, "May", 10,
  2013, "June", 10,
  2013, "July", 10,
  2013, "August", 8,
  2013, "September", 9,
  2013, "October", 6,
  2013, "November", 10,
  2013, "December", 10,
  2014, "January", 14,
  2014, "February", 13,
  2014, "March", 15,
  2014, "April", 12,
  2014, "May", 9,
  2014, "June", 6,
  2014, "July", 4,
  2014, "August", 14,
  2014, "September", 5,
  2014, "October", 20,
  2014, "November", 13,
  2014, "December", 7,
  2015, "January", 7,
  2015, "February", 14,
  2015, "March", 10,
  2015, "April", 9,
  2015, "May", 11,
  2015, "June", 6,
  2015, "July", 6,
  2015, "August", 10,
  2015, "September", 8,
  2015, "October", 13,
  2015, "November", 10,
  2015, "December", 9,
  2016, "January", 7,
  2016, "February", 17,
  2016, "March", 9,
  2016, "April", 8,
  2016, "May", 13,
  2016, "June", 8,
  2016, "July", 12,
  2016, "August", 14,
  2016, "September", 11,
  2016, "October", 12,
  2016, "November", 6,
  2016, "December", 18,
  2017, "January", 7,
  2017, "February", 8,
  2017, "March", 13,
  2017, "April", 15,
  2017, "May", 7,
  2017, "June", 9,
  2017, "July", 19,
  2017, "August", 10,
  2017, "September", 12,
  2017, "October", 13,
  2017, "November", 11,
  2017, "December", 17,
  2018, "January", 12,
  2018, "February", 10,
  2018, "March", 14,
  2018, "April", 14,
  2018, "May", 12,
  2018, "June", 14,
  2018, "July", 10,
  2018, "August", 14,
  2018, "September", 14,
  2018, "October", NA,
  2018, "November", NA,
  2018, "December", NA
)

data_radar <- movies_theatres  %>% 
  mutate(
    Year = as.character(Year),
    Month = factor(Month, levels = locale()$date_names$mon)
  ) %>% 
  bind_rows(
    movies_theatres  %>% 
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
    Year = factor(x = Year, levels = c(2013:2018, "ALL")),
    Month_int = as.integer(Month)
  )

p <- ggplot(data = data_radar, aes(group = Year)) +
  # theme
  theme_black(base_size = 16) +
  geom_hline(
    yintercept = seq(0, 20, by = 2.5),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2
  ) +
  geom_hline(
    yintercept = seq(0, 20, by = 5),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4
  ) +
  # geom_rect(
  #   fill = ggplot2::theme_get()$plot.background$colour,
  #   xmin = 1,
  #   xmax = 0,
  #   ymin = -Inf,
  #   ymax = Inf
  # ) +
  geom_hline(
    yintercept = 20,
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.4
  ) +
  geom_vline(
    xintercept = seq_len(12),
    colour = ggplot2::theme_get()$panel.grid$colour,
    size = 0.2
  ) +
  geom_text(
    data = tibble(
      Count = c(0, rep(seq(5, 20, by = 5), times = 4)),
      Month = c(1, rep(c(1, 4, 7, 10), each = 4))
    ),
    mapping = aes(x = Month, y = Count, label = Count),
    colour = ggplot2::theme_get()$text$colour,
    size = 16 * 1/4,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(limits = c(0, 22), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 12),
    breaks = seq_len(12),
    labels = locale()$date_names$mon,
    expand = c(0, 0)
  ) +
  coord_polar(theta = "x", start = 2 * pi * 11/12) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(0.80)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) +
  # real geom
  scale_colour_manual(
    name = "Year",
    values = c("white", viridis_pal(direction = 1)(length(2013:2018))) %>% 
      `names<-`(c("ALL", 2013:2018))
  ) +
  scale_fill_manual(
    name = "Year",
    values = c("white", viridis_pal(direction = 1)(length(2013:2018))) %>% 
      `names<-`(c("ALL", 2013:2018))
  ) +
  scale_size_manual(
    name = "Year",
    values = c(5, rep(3, length(2013:2018))) %>% 
      `names<-`(c("ALL", 2013:2018))
  ) +
  geom_vline(
    mapping = aes(xintercept = Month_int),
    colour = "grey50"
  ) +
  geom_point(
    mapping = aes(x = Month_int, y = Count, colour = Year, size = Year)
  ) +
  geom_path(
    mapping = aes(x = Month_int, y = Count, colour = Year),
    size = 1.5
  ) +
  labs(title = "# Movies Seen in Theatres") +
  labs(caption = "© Mickaël 'Coeos' Canouil") +
  theme(plot.caption = element_text(size = 8, hjust = 0.5))

animate(
  plot = p + 
    transition_reveal(id = Year, along = Month_int, range = c(0L, 13L)) +
    shadow_wake(
      wake_length = 0.15,
      wrap = FALSE,
      exclude_layer = c(1:5, 7:8),
      exclude_phase = NULL
    ), 
  width = 500, 
  height = 500, 
  units = "px", 
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gifski_renderer(
    file = "./images/movies_theatres.gif"
  )
)
