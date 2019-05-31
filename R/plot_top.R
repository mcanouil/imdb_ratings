plot_top <- function(.data, n = 3, gg_fontsize = 9, base_family) {
  .data %>% 
    dplyr::arrange(dplyr::desc(`Your Rating`), dplyr::desc(`IMDb Rating`)) %>%
    dplyr::select(Title, `Your Rating`, `IMDb Rating`) %>%
    utils::head(n) %>%
    ggpubr::ggtexttable(
      rows = NULL, 
      theme = ttheme(base_style = "mBlack", base_size = gg_fontsize*0.80, base_family = base_family)
    ) %>%
    gridExtra::arrangeGrob(
      top = ggpubr::text_grob(
        label = "Best Movies", 
        color = ggplot2::theme_get()$text$colour, 
        face = "bold", 
        size = gg_fontsize*1.2, 
        family = base_family
      )
    ) %>% 
    ggpubr::as_ggplot()
}
