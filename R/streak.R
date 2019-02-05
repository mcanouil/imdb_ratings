compute_streak <- function(vec) {
  max(subset(
    x = as.data.frame(unclass(rle(vec))), 
    subset = values, 
    select = lengths, 
    drop = TRUE
  ))
}

print_streak <- function(vec) {
  return(
    paste0(
      "Movies rated: ", sum(vec), "\n",
      "Best streak: ", compute_streak(vec != 0), " days ! ",
      "Worst streak: ", compute_streak(vec == 0), " days !"
    )
  )
}
