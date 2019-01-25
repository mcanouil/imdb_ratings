plot_genres_distribution <- function(.data) {
  .data %>% 
    dplyr::mutate(
      Genre_details = purrr::map(.x = Genres, .f = function(x) {
        dplyr::tibble(
          Genre = unique(unlist(strsplit(x, split = ", "))),
          Weight = 1 / length(Genre)
        )
      })
    ) %>% 
    tidyr::unnest(Genre_details) %>% 
    dplyr::group_by(Genre) %>% 
    dplyr::summarise(Score = sum(Weight)) %>% 
    dplyr::arrange(dplyr::desc(Score)) %>% 
    dplyr::filter(Genre!="NANA") %>% 
    dplyr::mutate(
      Genre_label = Score>=stats::quantile(Score, probs = 2/3),
      Genre_label = ifelse(Genre_label, Genre, NA),
      Genre = factor(Genre, levels = unique(Genre))
    ) %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(x = factor(1), y = Score)) +
      ggplot2::geom_bar(mapping = ggplot2::aes(fill = Genre), width = 1, stat = "identity", na.rm = TRUE) +
      ggrepel::geom_label_repel(
        mapping = ggplot2::aes(
          x = 1.5,
          y = cumsum(rev(Score)) - rev(Score) / 2,
          label = rev(Genre_label)
        ), 
        colour = "black",
        fill = "white",
        size = 2, 
        nudge_x = 0.25,
        min.segment.length = 0, 
        segment.colour = ggplot2::theme_get()$line$colour
      ) +
      ggplot2::coord_polar(theta = "y") +
      scale_fill_viridis_d(guide = ggplot2::guide_legend(ncol = 2)) +
      ggplot2::labs(title = "Distribution of Genres") +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(), 
        legend.text = ggplot2::element_text(size = ggplot2::rel(0.4))
      )
}
