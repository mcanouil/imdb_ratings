reprex::reprex({
### Load packages
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)


devtools::source_url("https://github.com/mcanouil/DEV/raw/master/R/theme_black.R")
devtools::source_url("https://github.com/mcanouil/DEV/raw/master/R/format.R")

base_family <- if ("sysfonts" %in% installed.packages()) "xkcd" else ""

theme_set(theme_black(base_size = 16, base_family = base_family))

source("/home/Coeos/data/IMDbRating/R/metascore.R")
all_movies_theatres <- source("/home/Coeos/data/IMDbRating/data/movies_theatres.R")$value
movies_theatres <- all_movies_theatres %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(Count = n)
ratings <- add_metascore(
  file_in = "/home/Coeos/data/IMDbRating/data/ratings.csv", 
  file_out = "/home/Coeos/data/IMDbRating/data/ratings_ms.csv", 
  from = min(movies_theatres[["Year"]]), 
  locale = locale(encoding = "Windows-1252")
)

# full_join(
#   inner_join(
#     x = all_movies_theatres %>% 
#       filter(Month %in% c("January", "February", "March", "April", "May", "June", "July")) %>%
#       count(Year, name = "In Theatres"),
#     y = ratings %>% 
#       filter(lubridate::month(`Date Rated`) %in% 1:7) %>%
#       count(YearRated, name = "Total"),
#     by = c("Year" = "YearRated")
#   ) %>% 
#     filter(Year != "2014"),
# 
#   inner_join(
#     x = all_movies_theatres %>% 
#       count(Year, name = "In Theatres"),
#     y = ratings %>% 
#       count(YearRated, name = "Total"),
#     by = c("Year" = "YearRated")
#   ) %>% 
#     filter(Year != "2014"),
#   by = "Year"
# ) %>% 
#   rename_all(~gsub(".[xy]$", "", .x)) %>% 
#   kable() %>% 
#   kable_styling(
#     bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#     full_width = FALSE,
#     position = "center",
#     font_size = 12
#   ) %>% 
#   add_header_above(c("", "January-July" = 2, "January-December" = 2))



inner_join(
  x = all_movies_theatres %>% 
    count(Year, Month, name = "In Theatres"),
  y = ratings %>% 
    mutate(Month = as.character(month(`Date Rated`, abbr = FALSE, label = TRUE))) %>% 
    count(YearRated, Month, name = "Total"),
  by = c("Year" = "YearRated", "Month" = "Month")
) %>% 
  filter(Year != "2014") %>% 
  mutate(Month = factor(Month, levels = locale()$date_names$mon)) %>% 
  arrange(Year, Month) %>% 
  group_by(Year) %>% 
  mutate(
    `In Theatres` = cumsum(`In Theatres`),
    Total = cumsum(Total)
  ) %>% 
  ungroup() %>% 
  # gather(key = "what", value = "count", -Year, -Month) %>% 
  ggplot(aes(x = as.numeric(Month), y = `In Theatres`, colour = factor(Year), fill = factor(Year))) +
  geom_point(
    aes(size = factor(Year)),
    na.rm = TRUE,
    shape = 21,
    colour = ggplot2::theme_get()$text$colour
  ) +
  geom_line() +
  # facet_grid(cols = vars(what)) +
  scale_x_continuous(labels = locale()$date_names$mon, breaks = 1:12) +
  scale_colour_viridis_d(begin = 0.4, direction = 1) +
  scale_fill_viridis_d(begin = 0.4, direction = 1) +
  scale_size_manual(values = c(1, 1, 1, 1, 2)*2) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    )
  ) +
  labs(x = NULL, y = "# Movies in Theatres", colour = "Year", fill = "Year") +
  guides(size = "none")
})
