options(stringsAsFactors = FALSE)

library(tidyverse)
library(scales)
library(gganimate)


circle_room_lille <- tibble(
  angle = seq(0, 360, by = 360 / 14) * pi / 180,
  radius = 2,
  x = radius * cos(angle),
  y = radius * sin(angle),
  room = (length(x) - 1):0
) %>% 
  slice(-n())

.data <- source("./data/movies_theatres.R")$value %>% 
  filter(theatre=="LILLE") %>% 
  mutate(watch = 1) %>% 
  group_by(date_time) %>% 
  complete(room = 1:14) %>% 
  replace_na(replace = list(watch = 0)) %>% 
  arrange(Year) %>%
  fill(everything()) %>% 
  ungroup() %>% 
  full_join(y = circle_room_lille, by = "room") %>% 
  arrange(date_time) %>% 
  group_by(room) %>% 
  do(mutate(., room_n = cumsum(watch))) %>% 
  ungroup() %>% 
  mutate(room = factor(room))

ggplot(
  data = .data, 
  mapping = aes(x = x, y = y)
) + 
  scale_x_continuous(expand = expand_scale(mult = c(0.2))) + 
  scale_y_continuous(expand = expand_scale(mult = c(0.2))) +
  scale_size_continuous(range = c(1, 25)) +
  scale_colour_viridis_c(option = "plasma") +
  scale_fill_viridis_d(option = "plasma", begin = 0.25) +
  coord_equal() +
  geom_line(
    data = filter(.data, watch==1), 
    mapping = aes(x = x, y = y, colour = as.numeric(date_time)), 
    # size = 1, 
    show.legend = FALSE, 
    inherit.aes = FALSE
  ) +
  geom_point(
    mapping = aes(fill = room, size = room_n, group = room),
    shape = 21,
    colour = "white"
  ) +
  geom_text(
    mapping = aes(size = room_n / 4, label = room, group = room), 
    show.legend = FALSE, 
    colour = "black"
  ) +
  transition_reveal(along = date_time) +
  theme(legend.position = "none")


