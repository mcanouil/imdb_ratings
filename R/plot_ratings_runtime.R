plot_ratings_runtime <- function(.data, base_family) {
  .data %>% 
    dplyr::select(`Your Rating`, `Runtime (mins)`) %>%
    dplyr::group_by(`Your Rating`) %>%
    dplyr::summarise(Runtime = sum(`Runtime (mins)`, na.rm = TRUE) / (60 * 24)) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = factor(1), y = Runtime)
    ) +
      ggplot2::geom_bar(mapping = ggplot2::aes(fill = factor(`Your Rating`)), width = 1, stat = "identity", na.rm = TRUE) +
      ggrepel::geom_label_repel(
        mapping = ggplot2::aes(
          x = 1.5,
          y = cumsum(rev(Runtime)) - rev(Runtime) / 2,
          label = round(rev(Runtime), digits = 1)
        ), 
        colour = "black",
        fill = "white", 
        size = 2, 
        nudge_x = 0.25,
        min.segment.length = 0, 
        segment.colour = "white",
        family = base_family
      ) +
      ggplot2::coord_polar(theta = "y") +
      scale_fill_viridis_d(name = "Rating") +
      ggplot2::labs(title = "Days Spent per Rating") +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        plot.title = ggplot2:element_text(hjust = 0.5),
        plot.subtitle = ggplot2:element_text(hjust = 0.5)
      )
}