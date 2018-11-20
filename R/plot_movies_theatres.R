plot_movies_theatres <- function(.year, .data) {
  month_factor <- factor(locale()$date_names$mon, levels = locale()$date_names$mon)
  .data %>% 
    mutate(
      Month = factor(Month, levels = levels(month_factor)),
      size = Year==.year
    ) %>% 
    ggplot() +
      geom_hline(yintercept = seq(0, 20, by = 2.5), colour = "white", size = 0.10) +
      geom_hline(yintercept = seq(0, 15, by = 5), colour = "white", size = 0.2) +
      geom_rect(fill = ggplot2::theme_get()$panel.background$fill, xmin = 1, xmax = 0, ymin = 0, ymax = 20) +
      geom_hline(yintercept = 20, colour = "white", size = 0.2) +
      geom_vline(aes(xintercept = as.integer(Month)), colour = "white", size = 0.1) +
      geom_text(
        data = tibble(
          Count = c(0, rep(seq(5, 20, by = 5), times = 4)), 
          Month = c(1, rep(c(1, 4, 7, 10), each = 4))
        ),
        aes(x = Month, y = Count, label = Count),
        colour = ggplot2::theme_get()$text$colour,
        size = 2.5
      ) +
      geom_smooth(
        aes(x = as.integer(Month), y = Count, colour = "ALL"),
        method = "loess",
        se = FALSE,
        size = 1,
        linetype = 2,
        show.legend = TRUE
      ) +
      geom_point(
        aes(
          x = as.integer(Month),
          y = Count,
          colour = factor(Year),
          size = size
        ),
        shape = 21
      ) +
      geom_line(
        aes(
          x = as.integer(Month),
          y = Count,
          group = Year,
          colour = factor(Year),
          size = size
        )
      ) +
      scale_colour_manual(
        name = "Year", 
        values = set_colours(.data[["Year"]])
      ) +
      scale_size_manual(values = c(0.15, 0.5)*1.5, guide = "none") +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
      scale_x_continuous(limits = c(0, 12), breaks = seq_len(12), labels = levels(month_factor)) +
      labs(title = "# Movies Seen in Theatres") +
      coord_polar(theta = "x", start = 2*pi * 22/24) +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(0.80)),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()
      )
}
