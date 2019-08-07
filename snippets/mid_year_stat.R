### Load packages
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)


devtools::source_url("https://github.com/mcanouil/DEV/raw/master/R/theme_black.R")
devtools::source_url("https://github.com/mcanouil/DEV/raw/master/R/format.R")

base_family <- if ("sysfonts" %in% installed.packages()) "xkcd" else ""

source("./R/metascore.R")
all_movies_theatres <- source("./data/movies_theatres.R")$value
movies_theatres <- all_movies_theatres %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(Count = n)
ratings <- add_metascore(
  file_in = "./data/ratings.csv", 
  file_out = "./data/ratings_ms.csv", 
  from = min(movies_theatres[["Year"]]), 
  locale = locale(encoding = "Windows-1252")
)

full_join(
  inner_join(
    x = all_movies_theatres %>% 
      filter(Month %in% c("January", "February", "March", "April", "May", "June", "July")) %>%
      count(Year, name = "In Theatres"),
    y = ratings %>% 
      filter(lubridate::month(`Date Rated`) %in% 1:7) %>%
      count(YearRated, name = "Total"),
    by = c("Year" = "YearRated")
  ) %>% 
    filter(Year != "2014"),

  inner_join(
    x = all_movies_theatres %>% 
      # filter(Month %in% c("January", "February", "March", "April", "May", "June", "July")) %>%
      count(Year, name = "In Theatres"),
    y = ratings %>% 
      # filter(lubridate::month(`Date Rated`) %in% 1:7) %>%
      count(YearRated, name = "Total"),
    by = c("Year" = "YearRated")
  ) %>% 
    filter(Year != "2014"),
  by = "Year"
) %>% 
  rename_all(~gsub(".[xy]$", "", .x)) %>% 
  kable() %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>% 
  add_header_above(c("", "January-July" = 2, "January-December" = 2))
