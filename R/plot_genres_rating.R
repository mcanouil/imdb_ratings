plot_genres_rating <- function(.data) {
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
    dplyr::summarise(
      Score = sum(Weight),
      Rating = (Weight %*% `Your Rating`) / Score,
      Rating_avg = mean(`Your Rating`),
      N = dplyr::n()
    ) %>% 
    dplyr::mutate(
      Per = N / sum(N)
    ) %>% 
    dplyr::arrange(dplyr::desc(Rating)) %>% 
    dplyr::filter(Genre!="NANA") %>% 
    dplyr::mutate(
      Genre = factor(Genre, levels = unique(Genre))
    ) %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(x = Genre, y = Rating, fill = N)) +
      ggplot2::geom_bar(width = 1, stat = "identity", colour = "white", na.rm = TRUE) +
      ggplot2::geom_label(
        mapping = ggplot2::aes(label = N), 
        colour = ggplot2::theme_get()$panel.grid$colour, 
        nudge_y = -0.5, 
        size = 2
      ) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(label = round(Rating, digits = 2)), 
        colour = ggplot2::theme_get()$text$colour, 
        nudge_y = 0.5, 
        size = 3
      ) +
      scale_fill_viridis_c(direction = 1) +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = seq(0, 10, 2)) +
      ggplot2::labs(title = "Average Weighted Rating per Genre", fill = "# Movies") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.title.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()
      )
}
