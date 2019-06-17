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



ggplot(data = circle_room_lille, mapping = aes(x = x, y = y)) + 
  geom_label(
    mapping = aes(label = sprintf("%02d", room)), 
    label.r = unit(2.35, "lines"), 
    label.padding =  unit(2, "lines")
  ) + 
  scale_x_continuous(expand = expand_scale(mult = c(0.2))) + 
  scale_y_continuous(expand = expand_scale(mult = c(0.2))) +
  coord_equal() +
  geom_point(
    data = source("./data/movies_theatres.R")$value %>% 
      filter(theatre=="LILLE") %>% 
      full_join(y = circle_room_lille, by = "room"),
    mapping = aes(colour = Year)
  ) + 
  geom_line(
    data = source("./data/movies_theatres.R")$value %>% 
      filter(theatre=="LILLE") %>% 
      full_join(y = circle_room_lille, by = "room"),
    mapping = aes(colour = Year)
  ) +
  transition_reveal(along = date_time)


source("./data/movies_theatres.R")$value %>% 
  filter(theatre=="LILLE") %>% 
  full_join(y = circle_room_lille, by = "room") %>% 
  arrange(date_time) %>% 
  group_by(room) %>% 
  mutate(room_n = 1:n()) %>% 
  ungroup() %>% 
  ggplot(
    data = ., 
    mapping = aes(x = x, y = y, colour = Year, size = room_n)
  ) + 
    scale_x_continuous(expand = expand_scale(mult = c(0.2))) + 
    scale_y_continuous(expand = expand_scale(mult = c(0.2))) +
    coord_equal() +
    geom_point() + 
    transition_reveal(along = date_time) +
    shadow_mark()
