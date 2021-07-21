library(here)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggtext)
library(showtext)

source(here("R/theme_black.R"))

font <- "Alegreya Sans"
font_add_google(font, font, regular.wt = 300)
showtext_auto()

streak_data <- fread(file = here("data", "theatres.csv"))[
  j = date := as.IDate(date_time)
][
  i = J(date = seq(min(date), today(), by = "1 day")), 
  on = "date"
][
  between(date, today() - months(12), today())
][
  j = list(
    count = sum(!is.na(theatre)),
    month = as.character(month(date, label = TRUE, abbr = FALSE)),
    year = year(date),
    month_num = month(date),
    wday = wday(date, label = TRUE, abbr = FALSE, week_start = 1),
    week = isoweek(date)
  ), 
  by = "date"
][
  i = order(date),
  j = `:=`(
    "x" = as.integer(factor(sprintf("x%s", week), levels = sprintf("x%s", unique(week)))),
    "y" = factor(wday, levels = rev(levels(wday)))
  )
]

week_month_breaks <- streak_data[
  i = wday == "Monday",
  j = list(year, week = x, n = .N),
  by = c("month_num", "month")
][
  j = month_start := week == week[which.min(abs(week - median(week)))],
  by = c("month_num", "month")
][
  i = (month_start),
  j = unique(.SD),
  .SDcols = c("month_num", "month", "week", "n")
][
  order(month_num, week)
]

p <- ggplot(data = streak_data) + 
  aes(x = x, y = y, label = count) +
  geom_tile(
    data = ~ .x[month_num %% 2 == 0],
    show.legend = FALSE,
    colour = "white",
    fill = "white",
    alpha = 0
  ) +
  geom_tile(
    data = ~ .x[month_num %% 2 == 1],
    show.legend = FALSE,
    colour = "white",
    fill = "white",
    alpha = 0.2
  ) +
  geom_tile(aes(fill = count), alpha = 0.3) +
  geom_text(
    data = ~ .x[count != 0],
    colour = "white", 
    na.rm = TRUE, 
    family = font, 
    size = 4
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0)),
    breaks = week_month_breaks[["week"]],
    labels = week_month_breaks[["month"]],
    guide = guide_axis(n.dodge = 2)
  ) +
  scale_y_discrete(expand = expansion(c(0, 0))) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  theme_mc_md(base_family = font) +
  theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )


svg(filename = "test.svg", width = 8, height = 2)
  print(p)
invisible(dev.off())
