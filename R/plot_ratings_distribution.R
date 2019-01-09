plot_ratings_distribution <- function(.data) {
  
  data_ratings <- dplyr::full_join(
    x = .data %>% 
      dplyr::select(dplyr::ends_with("Rating"), YearRated) %>%
      tidyr::gather(key = Who, value = Rating, -YearRated) %>%
      dplyr::mutate(
        Who = Who %>% gsub(" Rating", "", .) %>% gsub("Your", "User", .),
        rounded_rating = round(Rating, digits = 0)
      ) %>% 
      dplyr::select(-Rating) %>% 
      dplyr::group_by(YearRated, Who, rounded_rating) %>% 
      dplyr::summarise(n = dplyr::n()) %>% 
      dplyr::mutate(rating = factor(x = rounded_rating, levels = 1:10)) %>% 
      tidyr::complete(rating) %>% 
      dplyr::mutate(
        rounded_rating = ifelse(is.na(rounded_rating), as.numeric(rating), rounded_rating),
        n = ifelse(is.na(n), 0, n)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(YearRated, Who) %>% 
      dplyr::mutate(
        total = sum(n),
        ntotal = n/total
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(YearRated, Who, rounded_rating, ntotal),
    y = .data %>% 
      dplyr::select(dplyr::ends_with("Rating"), YearRated) %>%
      tidyr::gather(key = Who, value = Rating, -YearRated) %>%
      dplyr::mutate(
        Who = Who %>% gsub(" Rating", "", .) %>% gsub("Your", "User", .),
        rounded_rating = round(Rating, digits = 0)
      ) %>% 
      dplyr::select(-Rating),
    by = c("YearRated", "Who", "rounded_rating")
  )
  
  ggplot2::ggplot(data = data_ratings, mapping = ggplot2::aes(x = rounded_rating, fill = Who)) +
    ggplot2::geom_density(colour = "white", adjust = 2.5, alpha = 0.25, na.rm = TRUE) +
    ggplot2::geom_bar(
      data = dplyr::distinct(data_ratings),
      mapping = ggplot2::aes(y = ntotal),
      stat = "identity",
      colour = "white",
      width = 0.5,
      position = ggplot2::position_dodge(),
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0), 
      name = "Rating", 
      limits = c(0, 10), 
      breaks = c(0, seq_len(10))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expand_scale(mult = c(0, 0.1)), 
      labels = scales::percent
    ) +
    scale_fill_viridis_d() +
    ggplot2::labs(
      x = "Rating", 
      y = "Proportion", 
      title = "Distribution of Ratings", 
      fill = "Rating from"
    )
}
