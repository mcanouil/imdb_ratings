set_colours <- function(fac, direction = 1) {
  out <- viridis_pal(direction = direction)(dplyr::n_distinct(fac))
  names(out) <- sort(unique(as.character(fac)))
  out["SMOOTH"] <- "white"
  out["ALL"] <- "white"
  return(out)
}
