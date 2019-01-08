plot_movies_theatres <- function(.year, .data) {
  range_dates <- sort(unique(.data[["Year"]]))
  .data %>% 
    mutate(
      Month = map_if(.x = Month, .p = ~.x=="January", .f = ~c(.x, "JD")),
      size = Year==.year
    ) %>% 
    unnest() %>% 
    mutate(
      Year = as.character(Year),
      Month = factor(Month, levels = c(locale()$date_names$mon, "JD"))
    ) %>% 
    arrange(Year, Month) %>%
    mutate(
      YM = paste0(Year, sprintf("%02d", as.numeric(Month))) %>% factor() %>% as.integer(),
      Year = factor(x = Year, levels = c(range_dates, "ALL")),
      Month_int = as.integer(Month)
    ) %>% 
    ggplot(mapping = aes(group = Year)) +
    coord_polar(theta = "x") +
    # theme
    # theme_black(base_size = 16) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = rel(0.75)),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(), # panel.grid.minor.x
      panel.border = element_blank()
    ) +
    scale_y_continuous(
      limits = c(0, 22.5),
      breaks = seq(0, 20, by = 5),
      minor_breaks = seq(0, 20, by = 2.5),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = seq_len(12),
      labels = locale()$date_names$mon,
      expand = c(0, 0)
    ) +
    scale_colour_manual(
      name = "Year",
      values = set_colours(.data[["Year"]])
    ) +
    scale_fill_manual(
      name = "Year",
      values = set_colours(.data[["Year"]])
    ) +
    scale_size_manual(values = c(0.15, 0.5)*1.5, guide = "none") +
    geom_hline( # layer 1
      yintercept = seq(0, 20, by = 2.5),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.2
    ) +
    geom_hline( # layer 2
      yintercept = seq(0, 20, by = 5),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.4
    ) +
    geom_hline( # layer 3
      yintercept = 20,
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.4
    ) +
    geom_vline( # layer 4
      xintercept = seq_len(12),
      colour = ggplot2::theme_get()$panel.grid$colour,
      size = 0.2
    ) +
    geom_text( # layer 5
      data = tibble(
        Count = c(0, rep(seq(5, 20, by = 5), times = 4)), 
        Month = c(1, rep(c(1, 4, 7, 10), each = 4))
      ),
      mapping = aes(x = Month, y = Count, label = Count),
      colour = ggplot2::theme_get()$text$colour,
      size = 16 * 1/4,
      inherit.aes = FALSE
    ) +
    geom_vline( # layer 6
      mapping = aes(xintercept = Month_int),
      colour = "grey50"
    ) +
    geom_point( # layer 7
      mapping = aes(x = Month_int, y = Count, colour = Year, size = size)
    ) +
    geom_path( # layer 8
      mapping = aes(x = Month_int, y = Count, colour = Year, size = size)
    ) +
    labs(
      title = "# Movies Seen in Theatres"
    )
}
