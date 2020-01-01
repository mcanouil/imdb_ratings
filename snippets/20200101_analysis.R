library(tidyverse)

source("https://github.com/mcanouil/DEV/raw/master/R/theme_black.R")
# source("https://github.com/mcanouil/DEV/raw/master/R/format.R")

base_family <- if ("sysfonts" %in% installed.packages()) "xkcd" else ""
theme_set(theme_black(base_size = 9, base_family = base_family))

all_movies_theatres <- source("data/movies_theatres.R")$value %>% 
  dplyr::mutate(
    Month = factor(Month, levels = readr::locale(date_names = "en")$date_names$mon)
  ) %>% 
  dplyr::filter(Year > 2013)

imdb_ratings <- read_csv(file = "data/ratings.csv", locale = locale(encoding = "Windows-1252"))