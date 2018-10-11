plot_bad <- function(.data, n = 3, gg_fontsize = params$gg_fontsize) {
  .data %>% 
    arrange(`Your Rating`, `IMDb Rating`) %>%
    select(Title, `Your Rating`, `IMDb Rating`) %>%
    head(n) %>%
    ggtexttable(rows = NULL, theme = ttheme("mBlack", base_size = gg_fontsize*0.80)) %>%
    arrangeGrob(
      top = text_grob("Worst Movies", color = ggplot2::theme_get()$text$colour, face = "bold", size = gg_fontsize*1.2)
    ) %>% 
    as_ggplot()
}