library(tidyverse)
library(lubridate)


devtools::source_url('https://github.com/mcanouil/DEV/raw/master/R/theme_black.R')
theme_set(theme_black(base_size = 12))


ratings_day <- read_csv(file = "./data/ratings_ms.csv") %>% 
  mutate(day = wday(`Date Rated`, label = TRUE, abbr = FALSE, week_start = 1)) %>% 
  group_by(YearRated, day) %>% 
  summarise(
    n = n(),
    mean = mean(`Your Rating`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  group_by(YearRated) %>% 
  mutate(p = n / sum(n))

ggplot(data = ratings_day, mapping = aes(x = day, y = p, fill = factor(YearRated))) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent, expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = NULL, y = NULL, fill = "Year")
