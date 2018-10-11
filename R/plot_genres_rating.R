plot_genres_rating <- function(.data) {
  .data %>% 
    mutate(
      Genre_details = map(Genres, function(x) {
        tibble(
          Genre = unique(unlist(strsplit(x, split = ", "))),
          Weight = 1 / length(Genre)
        )
      })
    ) %>% 
    unnest(Genre_details) %>% 
    group_by(Genre) %>% 
    summarise(
      Score = sum(Weight),
      Rating = (Weight %*% `Your Rating`) / Score,
      Rating_avg = mean(`Your Rating`),
      N = n()
    ) %>% 
    mutate(
      Per = N / sum(N)
    ) %>% 
    arrange(desc(Rating)) %>% 
    filter(Genre!="NANA") %>% 
    mutate(
      Genre = factor(Genre, levels = unique(Genre))
    ) %>% 
    ggplot(aes(x = Genre, y = Rating, fill = N)) +
      geom_bar(width = 1, stat = "identity", colour = "white") +
      geom_text(aes(label = N), colour = ggplot2::theme_get()$panel.grid$colour, nudge_y = -0.5, size = 2) +
      geom_text(aes(label = round(Rating, digits = 2)), colour = ggplot2::theme_get()$text$colour, nudge_y = 0.5, size = 3) +
      scale_fill_viridis_c(direction = 1) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = seq(0, 10, 2)) +
      labs(title = "Average Weighted Rating per Genre", fill = "# Movies") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank()
      )
}
