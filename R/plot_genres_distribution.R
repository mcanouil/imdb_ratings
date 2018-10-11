plot_genres_distribution <- function(.data) {
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
    summarise(Score = sum(Weight)) %>% 
    arrange(desc(Score)) %>% 
    filter(Genre!="NANA") %>% 
    mutate(
      Genre_label = Score>=quantile(Score, probs = 2/3),
      Genre_label = ifelse(Genre_label, Genre, NA),
      Genre = factor(Genre, levels = unique(Genre))
    ) %>% 
    ggplot(aes(x = factor(1), y = Score, fill = Genre)) +
      geom_bar(colour = "white", width = 1, stat = "identity") +
      geom_label_repel(
        aes(
          x = 1.5,
          y = cumsum(rev(Score)) - rev(Score) / 2,
          label = rev(Genre_label)
        ), 
        fill = "white", 
        size = 2, 
        nudge_x = 0.25,
        min.segment.length = 0, 
        segment.colour = ggplot2::theme_get()$line$colour
      ) +
      coord_polar(theta = "y") +
      scale_fill_viridis_d(guide = guide_legend(ncol = 2)) +
      labs(title = "Distribution of Genres") +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(), 
        legend.text = element_text(size = rel(0.4))
      )
}
