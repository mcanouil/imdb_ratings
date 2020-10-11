### Environment ====================================================================================
library(here)


### Load packages ==================================================================================
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtext)
  library(patchwork)
  library(scales)
  library(glue)
  library(data.table)
  library(lubridate)
})


### Define theme ===================================================================================
source(here("R/theme_black.R"))

options(
  ggplot2.discrete.colour = function(...) scale_colour_viridis_d(..., begin = 1/3, end = 1),
  ggplot2.discrete.fill = function(...) scale_fill_viridis_d(..., begin = 1/3, end = 1),
  ggplot2.continuous.colour = function(...) scale_colour_viridis_c(..., begin = 1/3, end = 1),
  ggplot2.continuous.fill = function(...) scale_fill_viridis_c(..., begin = 1/3, end = 1)
)
theme_set(
  theme_black(11, base_family = "xkcd") +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_markdown(),
      plot.subtitle = element_markdown(face = "italic", size = rel(0.8)),
      plot.caption = element_markdown(face = "italic", size = rel(0.5)),
      axis.title.x = element_markdown(),
      axis.text.x = element_markdown(),
      axis.title.y = element_markdown(),
      axis.text.y = element_markdown()
    )
)

fig_caption <- "&copy; Micka&euml;l '<i style='color:#21908CFF;'>Coeos</i>' Canouil"


### Source functions ===============================================================================
get_metascore <- function(x) { 
  sapply(
    X = paste0("https://www.imdb.com/title/", x, "/"), 
    FUN = function(y) {
      as.numeric(rvest::html_text(
        x = rvest::html_node(
          x = xml2::read_html(x = paste0(y, "criticreviews")), 
          css = "div.metascore"
        )              
      )) / 10
    }
  )
}


### Data ===========================================================================================
movies_db <- fread(file = here("data", "theatres.csv"))[,
    date := as.IDate(date_time)
][
  J(date = seq(min(date), today(), by = "1 day")), 
  on = "date"
][,
  c("month", "year", "wday") := list(
    as.character(month(date, label = TRUE, abbr = FALSE)),
    year(date),
    wday(date, label = TRUE, abbr = FALSE, week_start = 1)
  )
][, 
  year_complete := uniqueN(date) >= 365, 
  by = "year"
]

ratings_db <- fread(file = here("data", "ratings.csv"), encoding = "Latin-1")
setnames(ratings_db, names(ratings_db), gsub("[()]", "", gsub(" ", "_", tolower(names(ratings_db)))))
ratings_db[, 
  c("date_rated", "release_date") := lapply(.SD, as.IDate), 
  .SDcols = c("date_rated", "release_date")
][, 
  c("month_rated", "year_rated") := list(
    month(date_rated, label = TRUE, abbr = FALSE),
    year(date_rated)
  )
]

if (file.exists(here("data", "ratings_ms.csv"))) {
  ratings_db_ms <- fread(file = here("data", "ratings_ms.csv"))[, 
    c("date_rated", "release_date") := lapply(.SD, as.IDate), 
    .SDcols = c("date_rated", "release_date")
  ][date_rated < (today() - 100) | year < year(today())]
  
  ratings_db_new <- ratings_db[
    !const %in% ratings_db_ms[["const"]]
  ][, metascore_rating := get_metascore(const)]
  
  fwrite(x = rbind(ratings_db_ms, ratings_db_new), file = here("data", "ratings_ms.csv"))
} else {
  fwrite(x = ratings_db[, metascore_rating := get_metascore(const)], file = here("data", "ratings_ms.csv"))
}

ratings_ms <- fread(file = here("data", "ratings_ms.csv"))[, 
  c("date_rated", "release_date") := lapply(.SD, as.IDate), 
  .SDcols = c("date_rated", "release_date")
][, 
  in_theatres := as.integer(const %in% movies_db[["imdb_id"]])
]

ratings <- merge(
  x = ratings_ms[, -c("year_rated", "month_rated")],
  y = movies_db[!is.na(imdb_id), .(const = imdb_id, year_rated = year, month_rated = month)][1:1000],
  by = "const",
  all = FALSE
)

ratings_long <- melt(
  data = ratings[, 
    c(grep("rating$", names(ratings), value = TRUE)) := lapply(.SD, as.numeric), 
    .SDcols = grep("rating$", names(ratings), value = TRUE)
  ], 
  id.vars = c("const", "year_rated"), 
  measure.vars = patterns("rating$"), 
  variable.name = "who", 
  value.name = "rating"
)[
  who != "metascore_rating"
][, 
  who := factor(
    x = toupper(gsub("your", "coeos", gsub("_.*", "", who))), 
    levels = c("COEOS", "IMDB"),
    labels = 1:2
  )
]


### Analysis =======================================================================================
who_colours <- setNames(viridis_pal(begin = 0.5, direction = -1)(2), c("COEOS", "IMDB USERS"))
  
text_legend <- glue_collapse(
  glue("<b style='color:{who_colours};'>{names(who_colours)}</b>"), 
  sep = ", ", 
  last = " and "
)

p <- ggplot(data = ratings_long) +
  aes(
    x = rating, 
    y = after_stat(count / tapply(count, group, sum)[group]),
    colour = who, 
    fill = who,
    group = who
  ) +
  annotation_raster(
    raster = matrix(
      data = rep(viridis_pal(option = "plasma", alpha = 0.10)(50), each = 50), 
      nrow = 50
    ),
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
    title = "Distribution of Ratings",
    subtitle = text_legend,
    caption = fig_caption
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(filename = here("images/1km-histogram.png"), plot = p, width = 6.3, height = 4.7, dpi = 240)

p <- ggplot(data = ratings_long[who == 1]) +
  aes(x = factor(year_rated), y = rating, fill = factor(year_rated), linetype = who) +
  annotation_raster(
    raster = matrix(
      data = rep(viridis_pal(option = "plasma", alpha = 0.10, begin = 0.15, direction = -1)(50), each = 50), 
      nrow = 50, byrow = TRUE
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  geom_violin(colour = "white", draw_quantiles = 0.5, trim = FALSE, na.rm = TRUE) +
  geom_hline(
    mapping = aes(yintercept = avg, linetype = who), 
    data = ~ .x[, list(avg = mean(rating)), by = "who"],
    linetype = 2,
    colour = "white"
  ) +
  stat_summary(
    fun = mean, 
    shape = 18, 
    colour = "white", 
    size = 1.5,
    na.rm = TRUE,
    position = position_dodge(width = 0.9)
  ) +
  scale_x_discrete(
    labels = function(x) {
      setNames(
        object = paste(
          "<b style='color:", viridis_pal(begin = 1/3, end = 1)(length(2013:2020)), ";'>", 
          2013:2020, 
          "</b>"
        ), 
        nm = 2013:2020
      )[x]
    }
  ) +
  scale_y_continuous(
    labels = function(x) {
      setNames(
        object = paste(
          "<b style='color:", viridis_pal(option = "plasma", begin = 0.15)(length(0L:10L)), ";'>", 
          0L:10L, 
          "</b>"
        ), 
        nm = 0L:10L
      )[as.character(x)]
    },
    limits = c(0L, 10L), 
    breaks = 0L:10L, 
    expand = c(0, 0)
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = NULL, y = "Rating", caption = fig_caption)

ggsave(filename = here("images/1km-violin.png"), plot = p, width = 6.3, height = 4.7, dpi = 240)

p <- ggplot(data = movies_db[, list(N = sum(!is.na(date_time))), by = c("year", "month")]) +
  aes(x = N) +
  geom_density(mapping = aes(y = after_stat(count), colour = factor(year))) +
  geom_density(mapping = aes(y = after_stat(count)), colour = "white") +
  geom_vline(
    mapping = aes(xintercept = avg), 
    data = ~ .x[, list(avg = mean(N))],
    linetype = 2,
    colour = "white"
  ) +
  scale_x_continuous(
    limits = c(0, NA), 
    breaks = 0:20,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)), 
    breaks = 0L:10L
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )  +
  labs(x = "Number of Movies per Month", y = "Count Density", caption = fig_caption)
  
ggsave(filename = here("images/1km-density.png"), plot = p, width = 6.3, height = 4.7, dpi = 240)
