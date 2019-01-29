plot_movies_theatres <- function(.year, .data, base_family) {
  range_dates <- sort(unique(.data[["Year"]]))
  .data %>% 
    dplyr::mutate(
      Month = purrr::map_if(.x = Month, .p = ~.x=="January", .f = ~c(.x, "JD")),
      size = Year==.year
    ) %>% 
    tidyr::unnest() %>% 
    dplyr::mutate(
      Year = as.character(Year),
      Month = factor(Month, levels = c(readr::locale()$date_names$mon, "JD"))
    ) %>% 
    dplyr::arrange(Year, Month) %>%
    dplyr::mutate(
      YM = paste0(Year, sprintf("%02d", as.numeric(Month))) %>% factor() %>% as.integer(),
      Year = factor(x = Year, levels = c(range_dates, "ALL")),
      Month_int = as.integer(Month)
    ) %>% 
    ggplot2::ggplot(mapping = ggplot2::aes(group = Year)) +
    ggplot2::coord_polar(theta = "x") +
    # theme
    # theme_black(base_size = 16) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(if(base_family=="xkcd"){1}else{0.75})),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), # panel.grid.minor.x
      panel.border = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 22.5),
      breaks = seq(0, 20, by = 5),
      minor_breaks = seq(0, 20, by = 2.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(12),
      labels = readr::locale()$date_names$mon,
      expand = c(0, 0)
    ) +
    ggplot2::scale_colour_manual(
      name = "Year",
      values = set_colours(.data[["Year"]])
    ) +
    ggplot2::scale_fill_manual(
      name = "Year",
      values = set_colours(.data[["Year"]])
    ) +
    ggplot2::scale_size_manual(values = c(0.15, 0.5)*1.5, guide = "none") +
    ggplot2::geom_hline( # layer 1
      yintercept = seq(0, 20, by = 2.5),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.2
    ) +
    ggplot2::geom_hline( # layer 2
      yintercept = seq(0, 20, by = 5),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.4
    ) +
    ggplot2::geom_hline( # layer 3
      yintercept = 20,
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.4
    ) +
    ggplot2::geom_vline( # layer 4
      xintercept = seq_len(12),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.2
    ) +
    ggplot2::geom_vline( # layer 5
      mapping = ggplot2::aes(xintercept = Month_int),
      colour = "grey50"
    ) +
    ggplot2::geom_text( # layer 6
      data = dplyr::tibble(
        Count = c(0, rep(seq(5, 20, by = 5), times = 4)), 
        Month = c(1, rep(c(1, 4, 7, 10), each = 4))
      ),
      mapping = ggplot2::aes(x = Month, y = Count, label = Count),
      colour = ggplot2::theme_get()$text$colour,
      size = 16 * 1/4,
      inherit.aes = FALSE,
      family = base_family
    ) +
    ggplot2::geom_point( # layer 7
      mapping = ggplot2::aes(x = Month_int, y = Count, colour = Year, size = size)
    ) +
    ggplot2::geom_path( # layer 8
      mapping = ggplot2::aes(x = Month_int, y = Count, colour = Year, size = size)
    ) +
    ggplot2::labs(
      title = "# Movies Seen in Theatres"
    )
}
