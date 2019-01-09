best_streak <- function(vec) {
  bs <- function(vec) {
    as.data.frame(unclass(rle(vec))) %>% 
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
