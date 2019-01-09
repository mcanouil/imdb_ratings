plot_top <- function(.data, n = 3, gg_fontsize = params$gg_fontsize) {
  .data %>% 
    dplyr::arrange(dplyr::desc(`Your Rating`), dplyr::desc(`IMDb Rating`)) %>%
    dplyr::select(Title, `Your Rating`, `IMDb Rating`) %>%
    utils::head(n) %>%
    ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme("mBlack", base_size = gg_fontsize*0.80)) %>%
    gridExtra::arrangeGrob(
      top = ggpubr::text_grob("Best Movies", color = ggplot2::theme_get()$text$colour, face = "bold", size = gg_fontsize*1.2)
    ) %>% 
    ggpubr::as_ggplot()
}
