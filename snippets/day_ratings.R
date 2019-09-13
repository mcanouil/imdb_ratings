library(tidyverse)
library(scales)
library(lubridate)


devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')
theme_set(theme_black(base_size = 12, base_family = "xkcd"))


ratings_day <- read_csv(file = "./data/ratings.csv", locale = locale(encoding = "Windows-1252")) %>%
  mutate(
    YearRated = year(`Date Rated`),
    Genres = map_chr(.x = Genres, .f = Hmisc::capitalize) %>%
      gsub("Musical", "Music", .)
  ) %>% 
  filter(YearRated >= 2017) %>% 
  mutate(day = wday(`Date Rated`, label = TRUE, abbr = FALSE, week_start = 1)) %>% 
  group_by(YearRated, day) %>% 
  summarise(
    n = n(),
    mean = mean(`Your Rating`, na.rm = TRUE)
  ) %>% 
  group_by(YearRated) %>% 
  mutate(
    p = n / sum(n)
  ) %>% 
  ungroup()

ggplot(data = ratings_day, mapping = aes(x = factor(YearRated), y = p, fill = day)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(
    mapping = aes(label = n), 
    position = position_dodge(width = 0.9), 
    hjust = -0.1, 
    angle = 90, 
    family = "xkcd"
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent, expand = expand_scale(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL, fill = "Year")
