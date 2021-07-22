library(here)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggtext)
library(showtext)
library(grDevices)

source(here("R", "theme_mc.R"))

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
    month = as.character(month(date, label = TRUE, abbr = TRUE)),
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
][
  j = month := sub("May.", "May", sprintf("%s.", month))
]

week_month_breaks <- streak_data[
  i = wday == "Monday",
  j = list(year, month, week = x)
][
  j = .SD[2, ],
  by = c("year", "month")
][
 !is.na(week)
]

p <- ggplot(data = streak_data) + 
  aes(x = x, y = y, label = count) +
  geom_tile(
    data = ~ .x[month_num %% 2 == 0],
    show.legend = FALSE,
    colour = NA, # "white",
    fill = "white",
    alpha = 0
  ) +
  geom_tile(
    data = ~ .x[month_num %% 2 == 1],
    show.legend = FALSE,
    colour = NA, # "white",
    fill = "white",
    alpha = 0.2
  ) +
  geom_tile(aes(fill = count), alpha = 0.3, colour = "#FAFAFA") +
  geom_richtext(
    data = ~ .x[count != 0],
    colour = "white", 
    na.rm = TRUE, 
    family = font, 
    fontface = "bold",
    size = 3.5,
    fill = NA, 
    label.colour = NA
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0)),
    breaks = week_month_breaks[["week"]],
    labels = week_month_breaks[["month"]],
    position = "top"
  ) +
  scale_y_discrete(
    expand = expansion(c(0, 0)),
    labels = function(x) sub("([[:alpha:]]{3}).*", "\\1.", x)
  ) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  theme_mc_md(base_family = font) +
  theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_markdown(face = "italic", size = rel(0.90))
  ) +
  labs(
    caption = sprintf(
      "<b>%s</b> movies seen in a <b>movie theatre</b> in the <b>last year</b>.", 
      format(sum(streak_data[["count"]]), digits = 0, big.mark = ",")
    )
  )

svg(filename = here("media", "streak.svg"), width = 8, height = 1.75)
  print(p)
invisible(dev.off())
