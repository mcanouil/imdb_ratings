plot_ratings_runtime <- function(.data) {
  .data %>% 
    select(`Your Rating`, `Runtime (mins)`) %>%
    group_by(`Your Rating`) %>%
    summarise(Runtime = sum(`Runtime (mins)`, na.rm = TRUE) / (60 * 24)) %>%
    ggplot(aes(x = factor(1), y = Runtime, fill = factor(`Your Rating`))) +
      geom_bar(colour = "white", width = 1, stat = "identity") +
      geom_label_repel(
        aes(
          x = 1.5,
          y = cumsum(rev(Runtime)) - rev(Runtime) / 2,
          label = round(rev(Runtime), digits = 1)
        ), 
        fill = "white", 
        size = 2, 
        nudge_x = 0.25,
        min.segment.length = 0, 
        segment.colour = "white"
      ) +
      coord_polar(theta = "y") +
      scale_fill_viridis_d(name = "Rating") +
      labs(title = "Days Spent per Rating") +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()
      )
}