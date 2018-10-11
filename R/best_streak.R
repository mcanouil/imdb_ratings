best_streak <- function(vec) {
  bs <- function(vec) {
    rle(vec) %>% 
      unclass() %>% 
      as.data.frame() %>% 
      subset(values, lengths) %>% 
      max()
  }
  paste0(
    "Movies rated: ", sum(vec), "\n",
    "Best streak: ", bs(vec != 0), " days ! ",
    "Worst streak: ", bs(vec == 0), " days !"
  ) %>% 
    return()
}
