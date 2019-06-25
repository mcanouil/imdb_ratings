options(stringsAsFactors = FALSE)

library(tidyverse)
library(scales)
library(gganimate)

devtools::source_url("https://github.com/mcanouil/DEV/raw/master/R/theme_black.R")

make_bubble_circle <- function(n, radius, count = 1) {
  radius <- radius + radius * count^2
  slice(
    tibble(
      angle = seq(0, 360, by = 360 / n) * pi / 180,
      r = radius,
      x = r * cos(angle),
      y = r * sin(angle),
      room = (length(x) - 1):0
    ),
    -n()
  )
}

movies_theatres <- source("./data/movies_theatres.R")$value

# theatre_room_observed <- movies_theatres %>% 
#   unite(col = "theatre_room", theatre, room, sep = "_") %>% 
#   .[["theatre_room"]]

theatre_room_observed <- movies_theatres %>% 
  group_by(theatre) %>% 
  summarise(room = list(1:max(room))) %>% 
  unnest() %>% 
  unite(col = "theatre_room", theatre, room, sep = "_") %>% 
  .[["theatre_room"]]

.data <- movies_theatres %>% 
  mutate(watch = 1) %>% 
  unite(col = "theatre_room", theatre, room, sep = "_", remove = FALSE) %>% 
  group_by(date_time) %>% 
  complete(theatre_room = theatre_room_observed) %>% 
  replace_na(replace = list(watch = 0)) %>% 
  arrange(Year) %>%
  fill(everything()) %>% 
  ungroup() %>% 
  arrange(date_time) %>% 
  group_by(theatre_room) %>% 
  do(mutate(., theatre_room_n = cumsum(watch))) %>% 
  ungroup()


circle_room <- movies_theatres %>% 
  count(theatre, room) %>% 
  group_by(theatre) %>% 
  summarise(
    max_room = max(room),
    max_n = max(n)
  ) %>% 
  arrange(max_n) %>% 
  mutate(theatre = factor(theatre, levels = unique(theatre))) %>% 
  mutate(
    circle = map2(
      .x = max_room, .y = as.numeric(theatre),
      .f = ~make_bubble_circle(n = .x, radius = 25, count = .y)
    )
  ) %>% 
  unnest() %>% 
  unite(col = "theatre_room", theatre, room, sep = "_")

# ggplot(data = circle_room, mapping = aes(x = x, y = y, colour = theatre)) + 
#   coord_equal() +
#   geom_point() +
#   theme(legend.position = "none")


.data_circle <- left_join(
  x = .data,
  y = circle_room, 
  by = c("theatre_room")
) 
  
ggplot(
  data = .data_circle, 
  mapping = aes(x = x, y = y)
) + 
  theme_black(base_size = 9) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x =  element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0.5)
  ) +
  scale_x_continuous(expand = expand_scale(mult = c(0.2))) + 
  scale_y_continuous(expand = expand_scale(mult = c(0.2))) +
  scale_size_continuous(range = c(1, 25)) +
  scale_colour_viridis_c(option = "viridis", direction = -1) +
  scale_fill_viridis_d(option = "plasma", begin = 0.25, direction = -1) +
  coord_equal() +
  # geom_line(
  #   data = filter(.data_circle, watch==1),
  #   mapping = aes(x = x, y = y, colour = as.numeric(date_time)),
  #   show.legend = FALSE,
  #   inherit.aes = FALSE
  # ) +
  geom_point(
    mapping = aes(fill = gsub("_.*", "", theatre_room), size = theatre_room_n, group = theatre_room),
    shape = 21,
    colour = "white", 
    na.rm = TRUE
  ) +
  geom_text(
    mapping = aes(size = theatre_room_n / 4, label = gsub(".*_", "", theatre_room), group = theatre_room),
    show.legend = FALSE,
    colour = "black"
  ) +
  theme(legend.position = "none") +
  # facet_wrap(facets = vars(theatre)) +
  transition_reveal(along = date_time)


