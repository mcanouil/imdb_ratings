get_metascore <- function(.x) { 
  sapply(.x, function(.y) {
    paste0(.y, "criticreviews") %>% 
    read_html() %>%
    html_node("div.metascore") %>%
    html_text() %>%
    as.numeric() %>% 
    `/`(10)
  })
}
