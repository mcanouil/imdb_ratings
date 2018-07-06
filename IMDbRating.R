options(stringsAsFactors = FALSE)

### Load packages
# library(parallel)
# library(kableExtra)
# library(Hmisc)
# library(broom)
# 
# library(cowplot)
# library(rvest)

library(tidyverse)
library(viridis)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggpubr)

invisible(sapply(list.files("../DEV/Rfunctions/", full.names = TRUE), source))


### ggplot2 parameters
gg_fontsize <- 9

theme_set(theme_black(base_size = gg_fontsize))
scale_colour_viridis <- hijack(
  scale_colour_viridis,
  option = "viridis",
  begin = 2 / 5,
  end = 1,
  direction = -1
)
scale_fill_viridis <- hijack(
  scale_fill_viridis,
  option = "viridis",
  begin = 2 / 5,
  end = 1,
  direction = -1
)
viridis_pal <- hijack(
  viridis_pal,
  option = "viridis",
  begin = 2 / 5,
  end = 1,
  direction = -1
)


### functions
set_colours <- function(fac, direction = 1) {
  fac %>% 
    unique() %>% 
    length() %>% 
    viridis_pal(direction = direction)(.) %>% 
    `names<-`(sort(unique(fac))) %>% 
    c("ALL" = "white", .)
}

bestStreak <- function(vec) {
  bs <- function(vec) {
    rle(vec) %>% 
      unclass() %>% 
      as.data.frame() %>% 
      subset(values, lengths) %>% 
      max()
  }
  paste0(
    "Movies rated: ", sum(vec), "\n",
    "Best streak: ", bs(vec != 0), " days ! ",
    "Worst streak: ", bs(vec == 0), " days !"
  ) %>% 
    return()
}

get_metascore <- function(.x) { 
  sapply(.x, function(.y) {
    paste0(.y, "criticreviews") %>% 
    read_html() %>%
    html_node("div.metascore") %>%
    html_text() %>%
    as.numeric() %>% 
    `/`(10)
  })
}

plot_streak <- function(.data) {
  .data <- .data %>%
    group_by(`Date Rated`) %>%
    summarise(
      avg_rating = mean(`Your Rating`),
      n_rating = n()
    ) %>%
    right_join(
      y = tibble(
        "Date Rated" = seq(
          from = as.Date(paste0(unique(year(.[["Date Rated"]])), "-01-01")),
          to = as.Date(paste0(unique(year(.[["Date Rated"]])), "-12-31")),
          by = 1
        )
      ),
      by = "Date Rated"
    ) %>%
    mutate(
      YearRated = year(`Date Rated`),
      Month = month(`Date Rated`, label = TRUE, abbr = FALSE),
      Day = day(`Date Rated`),
      wDay = wday(`Date Rated`, label = TRUE, abbr = FALSE, week_start = 1),
      wDay = factor(wDay, levels = rev(levels(wDay))),
      n_rating = ifelse(is.na(n_rating), 0, n_rating),
      Week = isoweek(`Date Rated`)
    ) %>%
    group_by(YearRated) %>%
    mutate(
      streak = bestStreak(n_rating),
      Week = ifelse(Month=="January" & Week>=50, 0, Week),
      Week = ifelse(Month=="December" & Week<=40, max(Week)+1, Week) 
    ) %>%
    ungroup()
  
  x_labels <- .data %>% 
    select(Week, Month) %>% 
    distinct() %>% 
    group_by(Month) %>% 
    summarise(Week = min(Week)+2)

  ggplot(data = .data, aes(x = Week, y = wDay, fill = avg_rating, label = n_rating)) +
    geom_tile(colour = "grey20", size = 0.1) +
    geom_text(colour = "white", fontface = "bold", size = 2) +
    scale_fill_viridis(name = "Average\nRating", limits = c(0, 10), na.value = "grey30") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0), 
      breaks = x_labels[["Week"]], 
      labels = x_labels[["Month"]]
    ) +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(title = "Movies Streak", x = "Month", y = "Day") +
    facet_grid(~streak)
}

plot_genres_distribution <- function(.data) {
  .data %>% 
    mutate(
      Genre_details = map(Genres, function(x) {
        tibble(
          Genre = unique(unlist(strsplit(x, split = ", "))),
          Weight = 1 / length(Genre)
        )
      })
    ) %>% 
    unnest(Genre_details) %>% 
    group_by(Genre) %>% 
    summarise(Score = sum(Weight)) %>% 
    arrange(desc(Score)) %>% 
    filter(Genre!="NANA") %>% 
    mutate(
      Genre_label = Score>=quantile(Score, probs = 2/3),
      Genre_label = ifelse(Genre_label, Genre, NA),
      Genre = factor(Genre, levels = unique(Genre))
    ) %>% 
    ggplot(aes(x = factor(1), y = Score, fill = Genre)) +
      geom_bar(colour = "white", width = 1, stat = "identity") +
      geom_label_repel(
        aes(
          x = 1.5,
          y = cumsum(rev(Score)) - rev(Score) / 2,
          label = rev(Genre_label)
        ), 
        fill = "white", 
        size = 2, 
        nudge_x = 0.25,
        min.segment.length = 0, 
        segment.colour = "white"
      ) +
      coord_polar(theta = "y") +
      scale_fill_viridis(discrete = TRUE, guide = guide_legend(ncol = 2)) +
      labs(title = "Distribution of Genres") +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(), 
        legend.text = element_text(size = rel(0.4))
      )
}

plot_genres_rating <- function(.data) {
  .data %>% 
    mutate(
      Genre_details = map(Genres, function(x) {
        tibble(
          Genre = unique(unlist(strsplit(x, split = ", "))),
          Weight = 1 / length(Genre)
        )
      })
    ) %>% 
    unnest(Genre_details) %>% 
    group_by(Genre) %>% 
    summarise(
      Score = sum(Weight),
      Rating = (Weight %*% `Your Rating`) / Score,
      Rating_avg = mean(`Your Rating`),
      N = n()
    ) %>% 
    mutate(
      Per = N / sum(N)
    ) %>% 
    arrange(desc(Rating)) %>% 
    filter(Genre!="NANA") %>% 
    mutate(
      Genre = factor(Genre, levels = unique(Genre))
    ) %>% 
    ggplot(aes(x = Genre, y = Rating, fill = N)) +
      geom_bar(width = 1, stat = "identity", colour = "white") +
      geom_text(aes(label = N), colour = "black", nudge_y = -0.5, size = 2) +
      geom_text(aes(label = round(Rating, digits = 2)), colour = "white", nudge_y = 0.5, size = 3) +
      scale_fill_viridis(discrete = FALSE, direction = 1) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = seq(0, 10, 2)) +
      labs(title = "Average Weighted Rating per Genre", fill = "# Movies") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank()
      )
}

plot_ratings_runtime <- function(.data) {
  .data %>% 
    select(`Your Rating`, `Runtime (mins)`) %>%
    group_by(`Your Rating`) %>%
    summarise(Runtime = sum(`Runtime (mins)`, na.rm = TRUE) / (60 * 24)) %>%
    ggplot(aes(x = factor(1), y = Runtime, fill = factor(`Your Rating`))) +
      geom_bar(colour = "white", width = 1, stat = "identity") +
      geom_label_repel(
        aes(
          x = 1.5,
          y = cumsum(rev(Runtime)) - rev(Runtime) / 2,
          label = round(rev(Runtime), digits = 1)
        ), 
        fill = "white", 
        size = 2, 
        nudge_x = 0.25,
        min.segment.length = 0, 
        segment.colour = "white"
      ) +
      coord_polar(theta = "y") +
      scale_fill_viridis(name = "Rating", discrete = TRUE) +
      labs(title = "Days Spent per Rating") +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()
      )
}

plot_ratings_distribution <- function(.data) {
  .data %>% 
    select(ends_with("Rating")) %>%
    gather(key = Who, value = Rating) %>%
    mutate(
      Who = Who %>% gsub(" Rating", "", .) %>% gsub("Your", "User", .),
      rounded_rating = round(Rating, digits = 0)
    ) %>% 
    ggplot(aes(x = round(Rating, digits = 0), fill = Who)) +
      geom_density(
        aes(x = Rating, y = (..count../sum(..count..))*100),
        bw = 1,
        alpha = 0.75,
        colour = "white"
      ) +
      geom_histogram(
        aes(y = ..count../sum(..count..)),
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
      scale_y_continuous(expand = c(0, 0), labels = percent, limits = c(0, 0.25)) +
      scale_fill_viridis(discrete = TRUE) +
      labs(
        x = "Rating", 
        y = "Proportion", 
        title = "Distribution of Ratings", 
        fill = "Rating from"
      )
}

plot_bad <- function(.data, n = 3) {
  .data %>% 
    arrange(`Your Rating`, `IMDb Rating`) %>%
    select(Title, `Your Rating`, `IMDb Rating`) %>%
    head(n) %>%
    ggtexttable(rows = NULL, theme = ttheme("mBlack", base_size = gg_fontsize*0.80)) %>%
    arrangeGrob(
      top = text_grob("Worst Movies", color = "white", face = "bold", size = gg_fontsize*1.2)
    ) %>% 
    as_ggplot()
}
  
plot_top <- function(.data, n = 3) {
  .data %>% 
    arrange(desc(`Your Rating`), desc(`IMDb Rating`)) %>%
    select(Title, `Your Rating`, `IMDb Rating`) %>%
    head(n) %>%
    ggtexttable(rows = NULL, theme = ttheme("mBlack", base_size = gg_fontsize*0.80)) %>%
    arrangeGrob(
      top = text_grob("Best Movies", color = "white", face = "bold", size = gg_fontsize*1.2)
    ) %>% 
    as_ggplot()
}

plot_movies_theatres <- function(.year, .data) {
  .data %>% 
    mutate(
      Month = factor(Month, levels = levels(month_factor)),
      size = Year==.year
    ) %>% 
    ggplot() +
      geom_hline(yintercept = seq(0, 20, by = 2.5), colour = "grey50", size = 0.2) +
      geom_hline(yintercept = seq(0, 20, by = 5), colour = "grey50", size = 0.4) +
      geom_rect(fill = "grey20", xmin = 1, xmax = 0, ymin = 0, ymax = 20) +
      geom_vline(aes(xintercept = as.integer(Month)), colour = "grey50", size = 0.2) +
      geom_text(
        data = tibble(Count = rep(seq(0, 20, by = 5), 4), Month = rep(c(1, 4, 7, 10), 5)),
        aes(x = Month, y = Count, label = Count),
        colour = "white",
        size = 2
      ) +
      geom_smooth(
        aes(x = as.integer(Month), y = Count, colour = "ALL"),
        method = "loess",
        se = FALSE,
        size = 0.5,
        linetype = 2,
        show.legend = TRUE
      ) +
      geom_point(
        aes(
          x = as.integer(Month),
          y = Count,
          colour = factor(Year),
          size = size
        ),
        shape = 21
      ) +
      geom_line(
        aes(
          x = as.integer(Month),
          y = Count,
          group = Year,
          colour = factor(Year),
          size = size
        )
      ) +
      scale_colour_manual(
        name = "Year", 
        values = set_colours(.data[["Year"]], direction = 1)
      ) +
      scale_size_manual(values = c(0.15, 0.5)*1.25, guide = "none") +
      scale_y_continuous(limits = c(0, 22), expand = c(0, 0)) +
      scale_x_continuous(limits = c(0, 12), breaks = seq_len(12), labels = levels(month_factor)) +
      labs(title = "# Movies Seen in Theatres") +
      coord_polar(theta = "x", start = 2*pi * 22/24) +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(0.80)),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()
      )
}

print_infography <- function(.x, .a, .b, .c, .d, .e, .g, .h, .i) {
  ggarrange(
    ggarrange(
      .b[[1]],
      ggarrange(
        .a[[1]],
        ggarrange(
          .i[[1]],
          ggarrange(
            .c[[1]], 
            .d[[1]], 
            ncol = 1,
            nrow = 2
          ),
          ncol = 2,
          widths = c(0.6, 0.40)
        ),
        nrow = 2,
        align = "v"
      ),
      ncol = 2,
      align = "v",
      widths = c(1/3, 2/3)
    ),
    ggarrange(
      .e[[1]],
      .h[[1]] + 
        guides(fill = guide_legend(
          ncol = 1, 
          keyheight = unit(gg_fontsize*2/3, "pt"),
          keywidth = unit(gg_fontsize, "pt")
        )), 
      .g[[1]] + 
        guides(fill = guide_legend(
          ncol = 1, 
          keyheight = unit(gg_fontsize*1.2, "pt"),
          keywidth = unit(gg_fontsize*1.2, "pt")
        )), 
      ncol = 3,
      align = "v",
      widths = c(0.5, 0.25, 0.25)
    ),
    nrow = 2,
    heights = c(2/3, 1/3),
    align = "v"
  ) %>%
    arrangeGrob(
      top = text_grob(
        paste0(.x, " in Movie Theatres"), 
        color = "white", 
        face = "bold", 
        size = 16
      ),
      left = "",
      right = "", 
      bottom = text_grob(
        "© Mickaël 'Coeos' Canouil", 
        color = "white", 
        face = "bold",
        hjust = -6.64,
        size = 5
      )
    ) %>%
    as_ggplot()
}


### data
month_factor <- factor(locale()$date_names$mon, levels = locale()$date_names$mon)
movies_theatres <- tribble(
  ~Year, ~Month, ~Count,
  2013, "January", NA,
  2013, "February", NA,
  2013, "March", NA,
  2013, "April", 8,
  2013, "May", 10,
  2013, "June", 10,
  2013, "July", 10,
  2013, "August", 8,
  2013, "September", 9,
  2013, "October", 6,
  2013, "November", 10,
  2013, "December", 10,
  2014, "January", 14,
  2014, "February", 13,
  2014, "March", 15,
  2014, "April", 12,
  2014, "May", 9,
  2014, "June", 6,
  2014, "July", 4,
  2014, "August", 14,
  2014, "September", 5,
  2014, "October", 20,
  2014, "November", 13,
  2014, "December", 7,
  2015, "January", 7,
  2015, "February", 14,
  2015, "March", 10,
  2015, "April", 9,
  2015, "May", 11,
  2015, "June", 6,
  2015, "July", 6,
  2015, "August", 10,
  2015, "September", 8,
  2015, "October", 13,
  2015, "November", 10,
  2015, "December", 9,
  2016, "January", 7,
  2016, "February", 17,
  2016, "March", 9,
  2016, "April", 8,
  2016, "May", 13,
  2016, "June", 8,
  2016, "July", 12,
  2016, "August", 14,
  2016, "September", 11,
  2016, "October", 12,
  2016, "November", 6,
  2016, "December", 18,
  2017, "January", 7,
  2017, "February", 8,
  2017, "March", 13,
  2017, "April", 15,
  2017, "May", 7,
  2017, "June", 9,
  2017, "July", 19,
  2017, "August", 10,
  2017, "September", 12,
  2017, "October", 13,
  2017, "November", 11,
  2017, "December", 17,
  2018, "January", 12,
  2018, "February", 10,
  2018, "March", 14,
  2018, "April", 14,
  2018, "May", 12,
  2018, "June", NA,
  2018, "July", NA,
  2018, "August", NA,
  2018, "September", NA,
  2018, "October", NA,
  2018, "November", NA,
  2018, "December", NA
)

# "http://www.imdb.com/user/ur56341222/ratings?start=1&view=compact"

ratings_old <- read_csv("ratings_ms.csv")

ratings <- read_csv("ratings.csv", locale = locale(encoding = "Windows-1252")) %>%
  filter(!Const%in%ratings_old[["Const"]]) %>% 
  mutate(
    YearRated = year(`Date Rated`),
    Genres = map(Genres, simpleCap) %>%
      gsub("Musical", "Music", .)
  ) %>%
  filter(YearRated>=2016)

if (nrow(ratings)>0) {
  ratings <- ratings%>%
    mutate(
      `Metascore Rating` = get_metascore(URL)
    ) %>%
    bind_rows(ratings_old) %>% 
    write_csv(path = "ratings_ms.csv")
} else {
  ratings <- read_csv("ratings_ms.csv")
}


### make plots
gg_ratings <- ratings %>% 
  group_by(YearRated) %>% 
  nest() %>% 
  mutate(
    p_movies_streak = map(.x = data, .f = plot_streak),
    p_genres_distribution = map(.x = data, .f = plot_genres_distribution),
    p_genres_rating = map(.x = data, .f = plot_genres_rating),
    p_ratings_runtime = map(.x = data, .f = plot_ratings_runtime),
    p_ratings_distribution = map(.x = data, .f = plot_ratings_distribution),
    p_bad = map(.x = data, .f = plot_bad),
    p_top = map(.x = data, .f = plot_top),
    data_agg = list(movies_theatres),
    p_movies_theatres = map2(.x = YearRated, .y = data_agg, .f = plot_movies_theatres)
  ) %>% 
  select(-data, -data_agg) %>%
  ungroup()

gg_ratings  %>% 
  mutate(Year = YearRated) %>% 
  group_by(Year) %>% 
  mutate(
    gg = map(
      .x = YearRated,
      .a = p_movies_streak,
      .b = p_movies_theatres,
      .c = p_top,
      .d = p_bad,
      .e = p_genres_rating,
      .g = p_ratings_runtime,
      .h = p_genres_distribution,
      .i = p_ratings_distribution,
      .f = print_infography
    ) %>% 
      map2(.x = YearRated, .y = ., .f = function(x, y) {
        ggsave(
          filename = paste0("./images/", x, "_Coeos_IMDb.png"),
          plot = y,
          width = 16,
          height = 9,
          dpi = 300
        )
      })
  ) 



### gganimate
year_radar <- function(data) {
  set_colours <- function(fac, direction = 1) {
    fac %>% 
      unique() %>% 
      length() %>% 
      viridis_pal(direction = direction)(.) %>% 
      `names<-`(sort(unique(fac))) %>% 
      c("ALL" = "white", .)
  }
  ggplot(data = data) +
    geom_hline(yintercept = seq(0, 20, by = 2.5), colour = "grey50", size = 0.2) +
    geom_hline(yintercept = seq(0, 20, by = 5), colour = "grey50", size = 0.4) +
    geom_rect(fill = "grey20", xmin = 1, xmax = 0, ymin = 0, ymax = 20, inherit.aes = FALSE) +
    geom_vline(xintercept = seq_len(12), colour = "grey50", size = 0.2) +
    geom_text(
      data = tibble(Count = rep(seq(0, 20, by = 5), 4), Month = rep(c(1, 4, 7, 10), 5)),
      aes(x = Month, y = Count, label = Count),
      colour = "white",
      size = 2,
      inherit.aes = FALSE
    ) +
    scale_y_continuous(limits = c(0, 22), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 12), breaks = seq_len(12), labels = locale()$date_names$mon) +
    coord_polar(theta = "x", start = 2 * pi * 11/12) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = rel(0.80)),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank()
    ) +
    geom_smooth(
      aes(x = as.integer(Month), y = Count, colour = "ALL"),
      method = "loess",
      se = FALSE,
      size = 0.5,
      linetype = 2,
      show.legend = TRUE
    ) +
    scale_colour_manual(
      name = "Year", 
      values = set_colours(data[["Year"]], direction = 1)
    ) 
}

dta <- movies_theatres %>% 
  mutate(
    Year = factor(Year),
    Month = factor(Month, levels = locale()$date_names$mon)
  ) %>% 
  arrange(Year, Month) %>% 
  mutate(
    YM = paste0(Year, sprintf("%02d", as.numeric(Month))) %>% factor() %>% as.integer()
  )


p <- year_radar(data = dta) +
  # geom_point(aes(x = as.integer(Month), y = Count, colour = Year, frame = Year), shape = 21) +
  geom_path(
    aes(x = as.integer(Month), y = Count, colour = Year, frame = Year))

library(gganimate)
gganimate(p, filename = "./images/Coeos_IMDb.gif")


