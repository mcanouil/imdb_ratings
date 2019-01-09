get_metascore <- function(.x) { 
  purrr::map(.x = .x, .f = function(.y) {
    out <- rvest::html_text(
      x = rvest::html_node(
        x = xml2::read_html(x = paste0(.y, "criticreviews")), 
        css = "div.metascore"
      )              
    )
    return(as.numeric(out)/10)
  })
}
