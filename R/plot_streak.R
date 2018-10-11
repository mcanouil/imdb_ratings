plot_streak <- function(.data, facet = TRUE, number = TRUE) {
  .data <- .data %>%
    group_by(`Date Rated`) %>%
    summarise(
      avg_rating = mean(`Your Rating`),
      n_rating = n()
    ) %>%
    right_join(
      y = tibble(
        "Date Rated" = seq(
          from = as.Date(paste0(min(unique(year(.[["Date Rated"]]))), "-01-01")),
          to = as.Date(paste0(max(unique(year(.[["Date Rated"]]))), "-12-31")),
          by = 1
        )
      ),
      by = "Date Rated"
    ) %>%
    mutate(
      YearRated = year(`Date Rated`),
      Month = month(`Date Rated`, label = TRUE, abbr = FALSE),
      Day = day(`Date Rated`),
      wDay = wday(`Date Rated`, label = TRUE, abbr = FALSE, week_start = 1),
      wDay = factor(wDay, levels = rev(levels(wDay))),
      n_rating = ifelse(is.na(n_rating), 0, n_rating),
      Week = isoweek(`Date Rated`)
    ) %>%
    group_by(YearRated) %>%
    mutate(
      streak = paste0("[", year(`Date Rated`), "] ", best_streak(n_rating)),
      Week = ifelse(Month=="January" & Week>=50, 0, Week),
      Week = ifelse(Month=="December" & Week<=40, max(Week)+1, Week) 
    ) %>%
    ungroup()
  
  x_labels <- .data %>% 
    select(Week, Month) %>% 
    distinct() %>% 
    group_by(Month) %>% 
    summarise(Week = min(Week))

  p_out <- ggplot(
    data = .data, 
    mapping = aes(x = Week, y = wDay, fill = avg_rating, label = n_rating)
  ) +
    geom_tile(colour = "white", size = 0.1) +
    scale_fill_viridis_c(
      name = "Average\nRating", 
      limits = c(0, 10), 
      na.value = ggplot2::theme_get()$panel.background$fill
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0), 
      breaks = x_labels[["Week"]], 
      labels = x_labels[["Month"]]
    ) +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(title = "Movies Streak", x = "Month", y = "Day")
  
  if (number) {
    p_out <- p_out + geom_text(colour = "white", fontface = "bold", size = 2)
  }
  if (facet) {
    p_out <- p_out + facet_grid(~streak)
  }
  return(p_out)
}
