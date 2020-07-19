### Environment ====================================================================================
library(here)


### Load packages ==================================================================================
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtext)
  library(tidyr)
  library(glue)
  library(scales)
  library(purrr)
  library(dplyr)
  library(readr)
})


### Define theme ===================================================================================
source(here("R/theme_black.R"))
options(
  ggplot2.discrete.colour = scale_colour_viridis_d,
  ggplot2.discrete.fill = scale_fill_viridis_d,
  ggplot2.continuous.colour = scale_colour_viridis_c,
  ggplot2.continuous.fill = scale_fill_viridis_c
)
theme_set(
  theme_black(11, base_family = "xkcd") + 
    theme(
      plot.title.position = "plot", 
      plot.caption.position = "plot",
      plot.title = element_markdown(),
      plot.subtitle = element_markdown(face = "italic"),
      plot.caption = element_markdown(face = "italic", size = rel(0.6))
    )
)

fig_caption <- NULL # "&copy; Micka&euml;l '<i style='color:#21908CFF;'>Coeos</i>' Canouil"


### Import Data ====================================================================================
movies_db <- map_df(
  .x = list.files(here("R"), "[0-9]{4}.R", full.names = TRUE), 
  .f = function(x) { 
    source(x)
    read_csv(
      file = here("data", gsub(".R$", ".csv", basename(x))), 
      col_types = cols(
        date_time = col_datetime(format = ""),
        room = col_double(),
        theatre = col_character(),
        imdb_id = col_character()
      )
    )
  }
) %>% 
  mutate(
    month = as.character(month(date_time, label = TRUE, abbr = FALSE)),
    year = year(date_time)
  )

ratings_ms <- read_csv(
  file = here("data", "ratings_ms.csv"), 
  col_types = cols(
    const = col_character(),
    your_rating = col_double(),
    date_rated = col_date(format = ""),
    title = col_character(),
    url = col_character(),
    title_type = col_character(),
    imdb_rating = col_double(),
    runtime_mins = col_double(),
    year = col_double(),
    genres = col_character(),
    num_votes = col_double(),
    release_date = col_date(format = ""),
    directors = col_character(),
    month_rated = col_character(),
    year_rated = col_double(),
    metascore_rating = col_double()
  )
) %>% 
  mutate(in_theatres = as.integer(const %in% movies_db[["imdb_id"]]))


### Plot Time ======================================================================================
ggdata <- pivot_longer(
  data = inner_join(x = ratings_ms, y = movies_db, by = c("const" = "imdb_id")), 
  cols = ends_with("rating"), 
  names_to = "who", 
  names_pattern = "(.*)_.*", 
  names_transform = list(
    who = ~ factor(
      x = toupper(gsub("your", "coeos", .x)), 
      levels = c("COEOS", "METASCORE", "IMDB"),
      labels = 1:3
    )
  ),
  values_to = "rating"
)
who_colours <- setNames(viridis_pal(begin = 0.5, direction = -1)(3), c("COEOS", "METASCORE", "IMDB USERS"))
text_legend <- glue_collapse(
  glue("<b style='color:{who_colours};'>{names(who_colours)}</b>"), 
  sep = ", ", 
  last = " and "
)
p_ratings <- ggplot(
  data = ggdata,
  mapping = aes(
    x = rating, 
    y = after_stat(count / tapply(count, group, sum)[group]),
    colour = who, 
    fill = who
  )
) +
  annotation_raster(
    raster = matrix(rep(viridis_pal(option = "plasma", alpha = 0.10)(50), each = 50), nrow = 50),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  geom_bar(
    position = position_dodge2(width = 0.90, preserve = "single"),
    na.rm = TRUE,
    colour = NA
  ) +
  geom_text(
    mapping = aes(label = after_stat(percent(count / tapply(count, group, sum)[group], suffix = " %", accuracy = 0.1))),
    stat = "count",
    position = position_dodge2(width = 0.90, preserve = "single"),
    family = "xkcd",
    angle = 90,
    size = 3,
    hjust = -0.1,
    vjust = 0.5,
    na.rm = TRUE,
    show.legend = FALSE
  ) +
  scale_x_binned(
    limits = c(0, 10),
    right = FALSE,
    labels = function(x) ifelse(x %in% 10, x, paste0(x, "+")),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = percent_format(suffix = " %"), expand = expansion(c(0, 0.15))) +
  scale_colour_manual(values = unname(who_colours), labels = names(who_colours)) +
  scale_fill_manual(values = unname(who_colours), labels = names(who_colours)) +
  labs(
    x = "Rating", 
    y = "Percentage of Movies", 
    colour = NULL,
    fill = NULL,
    title = "Distribution of Ratings  for Movies Seen in Movie Theatres",
    subtitle = glue(
      'From {glue_collapse(range(ggdata[["year_rated"]]), sep = "--")},',
      '{comma(n_distinct(ggdata[["const"]]))} movies seen and rated by:', 
      text_legend,
      .sep = " "
    )
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) + 
  aes(group = who)

ggsave(
  filename = here("images/ratings_in_theatres.png"), 
  plot = p_ratings,   
  width = 16,
  height = 12,
  units = "cm",
  dpi = 120
)
