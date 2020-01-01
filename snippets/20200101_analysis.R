library(tidyverse)
library(lubridate)
library(gganimate)

source("https://github.com/mcanouil/DEV/raw/master/R/theme_black.R")
# source("https://github.com/mcanouil/DEV/raw/master/R/format.R")

base_family <- if ("sysfonts" %in% installed.packages()) "xkcd" else ""
theme_set(theme_black(base_size = 9, base_family = base_family))

all_movies_theatres <- source("data/movies_theatres.R")$value %>% 
  filter(Year > 2015) %>% 
  mutate(
    Day = wday(date_time, label = TRUE, abbr = FALSE, locale = "en_GB", week_start = 1),
    Day = factor(Day, levels = rev(levels(Day))),
    Month = factor(Month, levels = locale(date_names = "en")$date_names$mon),
    Year = factor(Year),
    Hour = hour(date_time)
  )

imdb_ratings <- read_csv(file = "data/ratings.csv", locale = locale(encoding = "Windows-1252")) %>% 
  pivot_longer(cols = ends_with("Rating"), names_to = "Who", values_to = "Rating") %>% 
  filter(year(`Date Rated`) > 2015)


p1 <- ggplot(all_movies_theatres, aes(x = Month, y = Year, z = Year)) +
  stat_summary_2d(fun = length, colour = "white") +
  stat_summary_2d(
    mapping = aes(label = stat(value)), 
    fun = length, 
    geom = "text", 
    colour = "grey20"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c() +
  labs(x = NULL, y = NULL, fill = "# Movies") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  plot = p1,
  filename = "images/year_day.png", 
  width = 12,
  height = 6,
  units = "cm", 
  dpi = 300
)

p2 <- ggplot(all_movies_theatres, aes(x = factor(Hour), y = Day, z = Day)) +
  stat_summary_2d(fun = length, colour = "white") +
  stat_summary_2d(
    mapping = aes(label = stat(value)), 
    fun = length, 
    geom = "text", 
    colour = "grey20"
  ) +
  scale_x_discrete(expand = c(0, 0), labels = function(x) paste0(x, "h")) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(na.value = "white") +
  labs(x = NULL, y = NULL, fill = "# Movies", title = "Year: {closest_state}") +
  theme(panel.grid = element_blank()) +
  transition_states(Year, transition_length = 0, state_length = 2)

gganimate::animate(
  plot = p2,
  width = 12,
  height = 6,
  units = "cm", 
  res = 300,
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(file = "images/day_time.gif")
)


p3 <- ggplot(
  data = imdb_ratings, 
  mapping = aes(
    x = factor(round(Rating, 0), levels = 0:10), y = stat(count), 
    colour = Who, fill = Who
  )
) +
  geom_density(mapping = aes(x = Rating), adjust = 2, alpha = 0.5) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  scale_colour_viridis_d(na.value = "white") +
  scale_fill_viridis_d(na.value = "white") +
  labs(
    x = "Rating", 
    y = "# Movies", 
    colour = NULL, 
    fill = NULL, 
    title = "Year: {closest_state}"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0, 1), 
    legend.justification = c(-0.05, 1.05)
  ) +
  guides(alpha = "none", colour = "none") +
  transition_states(year(`Date Rated`), transition_length = 1, state_length = 2)
  
gganimate::animate(
  plot = p3,
  width = 12,
  height = 6,
  units = "cm", 
  res = 300,
  bg = ggplot2::theme_get()$plot.background$colour,
  renderer = gganimate::gifski_renderer(file = "images/count_rating.gif")
)

p4 <- ggplot(
  data = full_join( 
      x = imdb_ratings %>% 
        filter(Who == "Your Rating") %>% 
        count(year(`Date Rated`), name = "ALL"),
      y = count(all_movies_theatres, year(date_time), name = "THEATRE"),
      by = c("year(`Date Rated`)" = "year(date_time)")
    ) %>% 
      rename(Year = `year(\`Date Rated\`)`) %>% 
      mutate(TV = ALL - THEATRE) %>% 
      rename("THEATRE & TV" = ALL) %>% 
      pivot_longer(cols = - Year, names_to = "Where", values_to = "Count") %>% 
      mutate(
        Year = factor(Year, levels = sort(unique(Year), decreasing = TRUE))
      ),
  mapping = aes(x = Where, y = factor(Year), fill = Count, label = Count)
) +
  geom_tile() +
  geom_text(colour = "grey20") +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(na.value = "white") +
  labs(x = NULL, y = NULL, fill = "# Movies") +
  theme(panel.grid = element_blank()) 

ggsave(
  plot = p4,
  filename = "images/year_counts.png", 
  width = 12,
  height = 6,
  units = "cm", 
  dpi = 300
)
