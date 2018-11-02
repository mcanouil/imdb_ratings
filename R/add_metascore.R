add_metascore <- function(
  file_in = "./data/ratings.csv", 
  file_out = "./data/ratings_ms.csv", 
  from = 2016, 
  locale = locale(encoding = "Windows-1252")
) {
  suppressMessages({
    ratings <- read_csv(file = file_in, locale = locale) %>%
      mutate(
        YearRated = year(`Date Rated`),
        Genres = map(Genres, capitalise) %>%
          gsub("Musical", "Music", .)
      ) %>%
      filter(YearRated>=!!from)
        
    if (file.exists(file_out)) {
      ratings_old <- read_csv(file = file_out)
      ratings <- filter(.data = ratings, !Const%in%ratings_old[["Const"]])
    
      if (nrow(ratings)>0) {
        ratings <- ratings %>%
          mutate(`Metascore Rating` = get_metascore(URL)) %>%
          bind_rows(ratings_old) %>%
          write_csv(path = file_out)
      }
    } else {
      if (nrow(ratings)>0) {
        ratings <- ratings %>%
          mutate(`Metascore Rating` = get_metascore(URL)) %>%
          write_csv(path = file_out)
      }
    }
  })
  return(read_csv(file = file_out))
}
