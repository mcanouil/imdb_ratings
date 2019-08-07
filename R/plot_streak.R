plot_streak <- function(.data, facet = TRUE, number = TRUE, base_family) {
  .data <- .data %>%
    dplyr::group_by(`Date Rated`) %>%
    dplyr::summarise(
      avg_rating = mean(`Your Rating`),
      n_rating = dplyr::n()
    ) %>%
    dplyr::right_join(
      y = dplyr::tibble(
        "Date Rated" = seq(
          from = as.Date(paste0(min(unique(lubridate::year(.[["Date Rated"]]))), "-01-01")),
          to = as.Date(paste0(max(unique(lubridate::year(.[["Date Rated"]]))), "-12-31")),
          by = 1
        )
      ),
      by = "Date Rated"
    ) %>%
    dplyr::mutate(
      YearRated = lubridate::year(`Date Rated`),
      Month = lubridate::month(`Date Rated`, label = TRUE, abbr = FALSE),
      Day = lubridate::day(`Date Rated`),
      wDay = lubridate::wday(`Date Rated`, label = TRUE, abbr = FALSE, week_start = 1),
      wDay = factor(wDay, levels = rev(levels(wDay))),
      n_rating = ifelse(is.na(n_rating), 0, n_rating),
      Week = lubridate::isoweek(`Date Rated`)
    ) %>%
    dplyr::group_by(YearRated) %>%
    dplyr::mutate(
      streak = paste0("[", lubridate::year(`Date Rated`), "] ", print_streak(n_rating)),
      Week = ifelse(Month=="January" & Week>=50, 0, Week),
      Week = ifelse(Month=="December" & Week<=40, max(Week)+1, Week) 
    ) %>%
    dplyr::ungroup()
  
  x_labels <- .data %>% 
    dplyr::select(Week, Month) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(Month) %>% 
    dplyr::summarise(Week = min(Week))

  p_out <- ggplot2::ggplot(
    data = .data, 
    mapping = ggplot2::aes(x = Week, y = wDay, fill = avg_rating, label = n_rating)
  ) +
    ggplot2::geom_tile(colour = "white", size = 0.1) +
    scale_fill_viridis_c(
      name = "Average\nRating", 
      limits = c(0, 10), 
      na.value = ggplot2::theme_get()$panel.background$fill
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0), 
      breaks = x_labels[["Week"]], 
      labels = x_labels[["Month"]]
    ) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2:element_text(hjust = 0.5),
      plot.subtitle = ggplot2:element_text(hjust = 0.5)
    ) +
    ggplot2::labs(title = "Movies Streak", x = "Month", y = "Day")
  
  if (number) {
    p_out <- p_out + ggplot2::geom_text(colour = "white", fontface = "bold", size = 2, family = base_family)
  }
  if (facet) {
    p_out <- p_out + ggplot2::facet_grid(cols = vars(streak))
  }
  
  p_out
}
