# set_colours <- function(fac, direction = 1) {
#   fac %>% 
#     unique() %>% 
#     length() %>% 
#     viridis_pal(direction = direction)(.) %>% 
#     `names<-`(sort(unique(fac))) %>% 
#     c("ALL" = ggplot2::theme_get()$text$colour, .)
# }

set_colours <- function(fac, direction = 1) {
  out <- fac %>%
    as.character() %>% 
    unique() %>%
    length() %>%
    viridis_pal(direction = direction)(.) %>%
    `names<-`(sort(unique(as.character(fac)))) %>%
    c("SMOOTH" = "white", .)
  out["ALL"] <- "white"
  return(out)
}
