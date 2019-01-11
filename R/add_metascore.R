add_metascore <- function(
  file_in = "./data/ratings.csv", 
  file_out = "./data/ratings_ms.csv", 
  from = 2016, 
  locale = readr::locale(encoding = "Windows-1252")
) {
  suppressMessages({
    ratings <- readr::read_csv(file = file_in, locale = locale) %>%
      dplyr::mutate(
        YearRated = lubridate::year(`Date Rated`),
        Genres = purrr::map_chr(.x = Genres, .f = capitalise) %>%
          gsub("Musical", "Music", .)
      ) %>%
      dplyr::filter(YearRated>=!!from)
        
    if (file.exists(file_out)) {
      ratings_old <- readr::read_csv(file = file_out)
      ratings <- dplyr::filter(.data = ratings, !Const%in%ratings_old[["Const"]])
    
      if (nrow(ratings)>0) {
        ratings <- ratings %>%
          dplyr::mutate(`Metascore Rating` = get_metascore(URL)) %>% 
          unnest() %>%
          dplyr::bind_rows(ratings_old) %>%
          readr::write_csv(path = file_out)
      }
    } else {
      if (nrow(ratings)>0) {
        ratings <- ratings %>%
          dplyr::mutate(`Metascore Rating` = get_metascore(URL)) %>%
          unnest() %>%
          readr::write_csv(path = file_out)
      }
    }
  })
  return(readr::read_csv(file = file_out))
}
