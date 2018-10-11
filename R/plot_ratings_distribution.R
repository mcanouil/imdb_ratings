plot_ratings_distribution <- function(.data) {
  
  data_ratings <- full_join(
    x = .data %>% 
      select(ends_with("Rating"), YearRated) %>%
      gather(key = Who, value = Rating, -YearRated) %>%
      mutate(
        Who = Who %>% gsub(" Rating", "", .) %>% gsub("Your", "User", .),
        rounded_rating = round(Rating, digits = 0)
      ) %>% 
      select(-Rating) %>% 
      group_by(YearRated, Who, rounded_rating) %>% 
      summarise(n = n()) %>% 
      mutate(rating = factor(x = rounded_rating, levels = 1:10)) %>% 
      complete(rating) %>% 
      mutate(
        rounded_rating = ifelse(is.na(rounded_rating), as.numeric(rating), rounded_rating),
        n = ifelse(is.na(n), 0, n)
      ) %>% 
      ungroup() %>% 
      group_by(YearRated, Who) %>% 
      mutate(
        total = sum(n),
        ntotal = n/total
      ) %>% 
      ungroup() %>% 
      select(YearRated, Who, rounded_rating, ntotal),
    y = .data %>% 
      select(ends_with("Rating"), YearRated) %>%
      gather(key = Who, value = Rating, -YearRated) %>%
      mutate(
        Who = Who %>% gsub(" Rating", "", .) %>% gsub("Your", "User", .),
        rounded_rating = round(Rating, digits = 0)
      ) %>% 
      select(-Rating),
    by = c("YearRated", "Who", "rounded_rating")
  )
  
  ggplot(data = data_ratings, aes(x = rounded_rating, fill = Who)) +
    geom_density(colour = "white", adjust = 2.5, alpha = 0.25) +
    geom_histogram(
      data = distinct(data_ratings),
      aes(y = ntotal),
      stat = "identity",
      binwidth = 0.5,
      colour = "white",
      position = position_dodge(preserve = "single")
    ) +
    scale_x_continuous(
      expand = c(0, 0), 
      name = "Rating", 
      limits = c(0, 10), 
      breaks = c(0, seq_len(10))
    ) +
    scale_y_continuous(expand = c(0, 0), labels = percent, limits = c(0, 0.5)) +
    scale_fill_viridis_d() +
    labs(
      x = "Rating", 
      y = "Proportion", 
      title = "Distribution of Ratings", 
      fill = "Rating from"
    )
}
