options(stringsAsFactors = FALSE)

library(tidyverse)
library(scales)
library(gganimate)
library(ggforce)

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

theatres_names <- function(x) {
  c(
    "LILLE" = "UGC LILLE", 
    "LYON PART-DIEU" = "UGC LYON PART-DIEU", 
    "VDA" = "UGC VDA", 
    "METROPOLE" = "METROPOLE", 
    "MAJESTIC" = "MAJESTIC"
  )[x]
}

movies_theatres <- source("./data/movies_theatres.R")$value
# %>% 
#   mutate(theatre = theatres_names[theatre])

theatre_room_observed <- movies_theatres %>%
  group_by(theatre) %>%
  summarise(room = list(1:max(room))) %>%
  unnest() %>%
  unite(col = "theatre_room", theatre, room, sep = "_") %>%
  .[["theatre_room"]]

theatre_room_all <- tribble(
  ~theatre, ~room,
  "LILLE", 1:14, 
  "LYON PART-DIEU", 1:14, 
  "VDA", 1:12, 
  "METROPOLE", 1:4, 
  "MAJESTIC", 1:6
) %>% 
  unnest() %>% 
  unite(col = "theatre_room", theatre, room, sep = "_") %>% 
  .[["theatre_room"]]

.data <- movies_theatres %>% 
  mutate(watch = 1) %>% 
  unite(col = "theatre_room", theatre, room, sep = "_", remove = FALSE) %>% 
  group_by(date_time) %>% 
  complete(theatre_room = theatre_room_all) %>% 
  replace_na(replace = list(watch = 0)) %>% 
  arrange(Year) %>%
  fill(everything()) %>% 
  ungroup() %>% 
  arrange(date_time) %>% 
  group_by(theatre_room) %>% 
  do(mutate(., theatre_room_n = cumsum(watch))) %>% 
  ungroup() %>% 
  group_by(date_time) %>% 
  do(mutate(., theatre_room_n_r = rank(theatre_room_n, ties.method = "min"))) %>% 
  ungroup() %>% 
  mutate(theatre_room_n_r = factor(theatre_room_n_r))


circle_room <- movies_theatres %>% 
  count(theatre, room) %>%
  unite(col = "theatre_room", theatre, room, sep = "_") %>% 
  complete(theatre_room = theatre_room_all) %>% 
  separate(col = "theatre_room", into = c("theatre", "room"), sep = "_", convert = TRUE) %>% 
  replace_na(replace = list(n = 0)) %>% 
  group_by(theatre) %>% 
  summarise(
    max_room = max(room),
    max_n = max(n),
    max = max_n * max_room
  ) %>% 
  arrange(max) %>% 
  mutate(theatre = factor(theatre, levels = unique(theatre))) %>% 
  mutate(
    circle = map2(
      .x = max_room, .y = as.numeric(theatre),
      .f = ~make_bubble_circle(n = .x, radius = 0.75, count = .y)
    )
  ) %>% 
  unnest() %>% 
  mutate(theatre_num = as.numeric(theatre)) %>% 
  left_join(
    y = make_bubble_circle(n = 5, radius = 40, count = 0) %>% 
      select(room, x_centre = x, y_centre = y), 
    by = c("theatre_num" = "room")
  ) %>% 
  mutate(x = x + x_centre, y = y + y_centre) %>%  
  unite(col = "theatre_room", theatre, room, sep = "_") %>% 
  select(theatre_room, x, y)

# ggplot(data = circle_room, mapping = aes(x = x, y = y, colour = gsub("_.*", "", theatre_room))) +
#   coord_equal() +
#   geom_point()+
#   theme(legend.position = "none") 

.data_circle <- left_join(
  x = .data,
  y = circle_room, 
  by = c("theatre_room")
)

base_family <- if ("sysfonts" %in% installed.packages()) "xkcd" else ""
  
p <- ggplot(
  data = .data_circle,
  mapping = aes(x = x, y = y)
) + 
  theme_black(base_size = 12, base_family = base_family) +
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
  scale_size_continuous(range = c(1, 15)) +
  scale_colour_viridis_c(option = "viridis", direction = -1) +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  coord_equal() +
  geom_mark_ellipse(
    # data = .data_circle %>% select(theatre_room, x, y) %>% distinct(),
    mapping = aes(
      # x = x, 
      # y = y,
      group = theatres_names(gsub("_.*", "", theatre_room)),
      label = theatres_names(gsub("_.*", "", theatre_room))
    ), 
    # inherit.aes = FALSE,
    fill = "white",
    alpha = 0.05,
    colour = "white",
    label.fill = "transparent",
    label.colour = "white",
    con.colour = "white",
    con.cap = unit(0, "mm"),
    expand = unit(7, "mm"), 
    label.family = base_family
  ) +
  geom_line(
    data = filter(.data_circle, watch==1),
    mapping = aes(x = x, y = y),
    show.legend = FALSE,
    inherit.aes = FALSE,
    colour = "grey50"
  ) +
  geom_point(
    mapping = aes(
      fill = theatre_room_n_r, 
      size = theatre_room_n, 
      group = theatre_room
    ),
    shape = 21,
    colour = "white", 
    na.rm = TRUE
  ) +
  geom_text(
    mapping = aes(
      size = theatre_room_n / 4, 
      label = gsub(".*_", "", theatre_room), 
      group = theatre_room
    ),
    show.legend = FALSE,
    colour = "black",
    family = base_family
  ) +
  theme(legend.position = "right") +
  guides(
    # size = guide_legend(
    #   title.position = "top", 
    #   title.hjust = 0.5, 
    #   label.position = "bottom"
    # ),
    colour = "none", 
    fill = "none"
  ) + 
  labs(
    title = "{frame_along}", 
    size = "# Movies", 
    caption = "© Mickaël 'Coeos' Canouil"
  ) +
  theme(
    plot.caption = element_text(size = rel(5 / 6), hjust = 0.5)
  )

gganimate::animate(
  plot = p + transition_reveal(along = date_time),
  width = 6.3 * 100,
  height = 4.7 * 100,
  units = "px",
  bg = p$theme$plot.background$colour,
  renderer = gganimate::gifski_renderer(
    file = "./images/Coeos_theatres.gif"
  )
)
