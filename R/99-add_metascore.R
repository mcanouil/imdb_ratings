library(purrr)
library(rvest)
library(readr)
library(xml2)
library(dplyr)
library(here)

get_metascore <- function(x) { 
  purrr::map(.x = x, .f = function(y) {
    message(y)
    out <- rvest::html_text(
      x = rvest::html_node(
        x = xml2::read_html(x = paste0(y, "criticreviews")), 
        css = "div.metascore"
      )              
    )
    
    as.numeric(out)/10
  })
}

add_metascore <- function(
  file_in = "ratings.csv", 
  file_out = "ratings_ms.csv", 
  from = 2016, 
  locale = readr::locale(encoding = "Windows-1252")
) {
  ratings <- readr::read_csv(file = file_in, locale = locale) %>%
    dplyr::mutate(
      YearRated = year(`Date Rated`),
      Genres = tolower(Genres)
    )  %>%
    dplyr::filter(YearRated >= !!from)
      
  if (file.exists(file_out)) {
    ratings_old <- readr::read_csv(file = file_out)
    ratings <- dplyr::filter(.data = ratings, !Const%in%ratings_old[["Const"]])
  
    if (nrow(ratings) > 0) {
      ratings <- ratings %>%
        dplyr::mutate(`Metascore Rating` = get_metascore(URL)) %>% 
        tidyr::unnest() %>%
        dplyr::bind_rows(ratings_old) %>%
        readr::write_csv(path = file_out)
    }
  } else {
    if (nrow(ratings) > 0) {
      ratings <- ratings %>%
        dplyr::mutate(`Metascore Rating` = get_metascore(URL)) %>%
        tidyr::unnest() %>%
        readr::write_csv(path = file_out)
    }
  }
  
  readr::read_csv(file = file_out)
}


