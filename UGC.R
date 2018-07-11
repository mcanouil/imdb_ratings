options(stringsAsFactors = FALSE)

### Load packages
# library(parallel)
# library(kableExtra)
# library(Hmisc)
# library(broom)
# 
# library(cowplot)

library(tidyverse)
library(viridis)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(rvest)

invisible(sapply(list.files("../DEV/Rfunctions/", full.names = TRUE), source))


### ggplot2 parameters
gg_fontsize <- 12

theme_set(theme_black(base_size = gg_fontsize))
scale_colour_viridis <- hijack(
  scale_colour_viridis,
  option = "viridis",
  begin = 2 / 5,
  end = 1,
  direction = -1
)
scale_fill_viridis <- hijack(
  scale_fill_viridis,
  option = "viridis",
  begin = 2 / 5,
  end = 1,
  direction = -1
)
viridis_pal <- hijack(
  viridis_pal,
  option = "viridis",
  begin = 2 / 5,
  end = 1,
  direction = -1
)

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
  2018, "June", NA,
  2018, "July", NA,
  2018, "August", NA,
  2018, "September", NA,
  2018, "October", NA,
  2018, "November", NA,
  2018, "December", NA
)
### gganimate
year_radar <- function(data, base_size = 10) {
  set_colours <- function(fac, direction = 1) {
    fac %>% 
      unique() %>% 
      length() %>% 
      viridis_pal(direction = direction)(.) %>% 
      `names<-`(sort(unique(fac))) %>% 
      c("ALL" = "white", .)
  }
  ggplot(data = data) +
    theme_black(base_size = base_size) +
    geom_hline(yintercept = seq(0, 20, by = 2.5), colour = "grey50", size = 0.2) +
    geom_hline(yintercept = seq(0, 20, by = 5), colour = "grey50", size = 0.4) +
    geom_rect(fill = "grey20", xmin = 1, xmax = 0, ymin = 0, ymax = 20, inherit.aes = FALSE) +
    geom_vline(xintercept = seq_len(12), colour = "grey50", size = 0.2) +
    geom_text(
      data = tibble(Count = rep(seq(0, 20, by = 5), 4), Month = rep(c(1, 4, 7, 10), 5)),
      aes(x = Month, y = Count, label = Count),
      colour = "white",
      size = base_size * 1/5,
      inherit.aes = FALSE
    ) +
    scale_y_continuous(limits = c(0, 22), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 12), breaks = seq_len(12), labels = locale()$date_names$mon) +
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
    geom_smooth(
      aes(x = as.integer(Month), y = Count, colour = "ALL"),
      method = "loess",
      se = FALSE,
      size = 0.5,
      linetype = 2,
      show.legend = TRUE
    ) +
    scale_colour_manual(
      name = "Year", 
      values = set_colours(data[["Year"]], direction = 1)
    ) 
}

dta <- movies_theatres %>% 
  mutate(
    Year = factor(Year),
    year = as.character(Year),
    Month = factor(Month, levels = locale()$date_names$mon)
  ) %>% 
  arrange(Year, Month) %>% 
  mutate(
    YM = paste0(Year, sprintf("%02d", as.numeric(Month))) %>% factor() %>% as.integer()
  )


library(gganimate)
p <- year_radar(data = dta, base_size = 16) +
  geom_point(
    aes(x = as.integer(Month), y = Count, colour = Year, frame = year, cumulative = TRUE), 
    shape = 21
  ) +
  geom_path(
    aes(x = as.integer(Month), y = Count, colour = Year, frame = year, cumulative = TRUE)
  ) +
  guides(colour = "none")
gganimate(
  p = p, 
  filename = "./images/Coeos_IMDb.gif", 
  ani.width = 500, 
  ani.height = 500, 
  ani.res = 100, 
  interval = 1
)

