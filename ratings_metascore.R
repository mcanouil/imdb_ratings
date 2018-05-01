library(parallel)
library(rvest)
library(tidyverse)

get_metascore <- function(.x) { 
  lapply(.x, function(.y) {
    paste0(.y, "criticreviews") %>% 
    read_html() %>%
    html_node("div.metascore") %>%
    html_text() %>%
    as.numeric() %>% 
    `/`(10)
  }) %>% 
    unlist()
}



ratings <- read_csv("ratings.csv", locale = locale(encoding = "Windows-1252")) %>% 
  group_by(cut(seq_along(URL), breaks = 100)) %>% 
  nest()

all <- mclapply(ratings[["data"]], function(idata) {
  data.frame(
    URL = idata[["URL"]],
    `Metascore Rating` = get_metascore(idata[["URL"]])
  )
}) 

write_rds(x = all, path = "ratings.rds")
