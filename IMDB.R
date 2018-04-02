#---------------------------------------------------------------------------------------------------
# Name -
# Desc -
# Author - Mickaël Canouil
# Date - 2017-09-01
#---------------------------------------------------------------------------------------------------
###############
### Settings
    rm(list = ls())
    options(stringsAsFactors = FALSE)
    # Sys.setlocale("LC_TIME", "English")
    
    
    library(tidyverse)
    library(readxl)
    library(writexl)
    library(broom)
    library(grid)
    library(scales)
    library(cowplot)
    library(ggrepel)
    library(viridis)
    
    sapply(list.files("../Rscripts/", full.names = TRUE), source) %>% invisible
    
    theme_dark <- TRUE
    if (theme_dark) {
        ## Theme Black ON ##
        theme_set(theme_black(base_size = 8))
        plot_grid <- hijack(plot_grid, theme_dark = theme_dark)
        scale_colour_viridis <- hijack(scale_colour_viridis, option = "viridis", begin = 2/5, end = 1, direction = -1)
        scale_fill_viridis <- hijack(scale_fill_viridis, option = "viridis", begin = 2/5, end = 1, direction = -1)
    } else {
        theme_set(theme_light(base_size = 8))
        scale_colour_viridis <- hijack(scale_colour_viridis, option = "viridis", begin = 0, end = 4/5)
        scale_fill_viridis <- hijack(scale_fill_viridis, option = "viridis", begin = 0, end = 4/5)
    }

    # ggsave(file = "Pictures/.png", plot = p, width = 7.5, height = 6, units = "in", dpi = 300)
    # ggsave(file = "Pictures/.png", plot = p, width = 12, height = 6, units = "in", dpi = 300)
    
    library(lubridate)



#####################
### Read ratings.csv
    bestStreak <- function (vec) {
        max(subset(as.data.frame(unclass(rle(vec))), values, lengths))
    }

    ratings <- read.csv("ratings.csv") # "http://www.imdb.com/user/ur56341222/ratings?start=1&view=compact"
    ratings[, "Genres"] <- sapply(ratings[, "Genres"], simpleCap)
    ratings[, "Date"] <- as.Date(
        x = gsub("(^.* )([^ ]*) ([0-9]*)$", "\\1\\3", ratings[, "Date.Added"]),
        format = "%a %b %d %H:%M:%S %Y", 
        tz = gsub("(^.* )([^ ]*) ([0-9]*)$", "\\2", ratings[, "Date.Added"])
    )

    ratings.agg <- ratings %>% group_by(Date) %>% summarise(avg.rating = mean(Your.Rating), n.rating = length(Your.Rating)) %>% as.data.frame
    ratings.agg <- merge(ratings.agg, 
        data.frame(Date = seq(from = as.Date("2015-01-01"), to = min(Sys.Date(), as.Date("2017-11-09")), by = 1)), 
        by = "Date", 
        all.y = TRUE
    )
    ratings.agg[, "Date.Added"] <- NULL
    ratings.agg[, "Year"] <- year(ratings.agg[, "Date"])
    ratings.agg[, "Month"] <- month(ratings.agg[, "Date"])
    ratings.agg[, "Day"] <- day(ratings.agg[, "Date"])
    ratings.agg[, "wDay"] <- wday(ratings.agg[, "Date"], label = TRUE, abbr = FALSE)
    ratings.agg[, "wDay"] <- factor(ratings.agg[, "wDay"], levels = c(levels(ratings.agg[, "wDay"])[-1], levels(ratings.agg[, "wDay"])[1]))
    ratings.agg <- ratings.agg[order(ratings.agg[, "Date"]), ]
    ratings.agg[, "fDay"] <- as.factor(paste(ratings.agg[, "wDay"], ratings.agg[, "Day"]))

    ratings.agg[is.na(ratings.agg[, "n.rating"]), "n.rating"] <- 0

    ratings.agg <- do.call("rbind", by(ratings.agg, ratings.agg[, "Year"], function (idta) {
        idta[, "Week"] <- c(
            rep(0, grep("Monday", idta[, "wDay"])[1]-1),
            rep(seq_along(grep("Monday", idta[, "wDay"])), each = 7)
        )[seq_len(nrow(idta))]
        idta[, "StreakYear"] <- paste0(
            "[", idta[, "Year"], "] Best streak: ",
            bestStreak(subset(ratings.agg, Year==unique(idta[, "Year"]))[, "n.rating"]!=0), " days ! ",
            "Worst streak: ", bestStreak(subset(ratings.agg, Year==unique(idta[, "Year"]))[, "n.rating"]==0), " days !"
        )
        return(idta)
    }))

    xlabels <- unique(cbind.data.frame(Month = month.abb[ratings.agg[, "Month"]], Week = ratings.agg[, "Week"], Year = ratings.agg[, "Year"]))
    p <- ggplot(data = ratings.agg, aes(x = Week, y = as.integer(wDay), fill = avg.rating, label = n.rating)) +
        geom_tile(colour = "grey20", size = 0.1) +
        geom_text(colour = "white", fontface = "bold", size = rel(2)) +
        facet_wrap(~StreakYear, ncol = 1) +
        scale_fill_viridis(name = "Average\nRating", limits = c(0, 10), na.value = "grey30") +
        scale_y_continuous(trans = "reverse", breaks = seq_len(7), labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), expand = c(0, 0)) +
        scale_x_continuous(breaks = xlabels[!duplicated(xlabels[, "Month"]), "Week"], labels = xlabels[!duplicated(xlabels[, "Month"]), "Month"], expand = c(0, 0)) +
        labs(title = "Movies streak", x = "Month", y = "Day") +
        theme(
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank(),
            panel.grid = element_blank()
        )
    ggsave(file = "RatingsCoeos.png", plot = p, width = 8, height = 4, units = "in", dpi = 300)

    plist <- lapply(unique(ratings.agg[, "Year"]), function (iyear) {
        xlabels <-    subset(unique(cbind.data.frame(Month = month.abb[ratings.agg[, "Month"]], Week = ratings.agg[, "Week"], Year = ratings.agg[, "Year"])), Year==iyear)
        p <- ggplot(data = subset(ratings.agg, Year==iyear), aes(x = Week, y = as.integer(wDay), fill = avg.rating, label = n.rating))+
            geom_tile(colour = "grey20", size = 0.1) +
            geom_text(colour = "white", fontface = "bold", size = rel(2)) +
            facet_wrap(~StreakYear, ncol = 1) +
            scale_fill_viridis(name = "Average\nRating", limits = c(0, 10), na.value = "grey30") +
            scale_y_continuous(trans = "reverse", breaks = seq_len(7), labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), expand = c(0, 0)) +
            scale_x_continuous(breaks = xlabels[!duplicated(xlabels[, "Month"]), "Week"], labels = xlabels[!duplicated(xlabels[, "Month"]), "Month"], expand = c(0, 0)) +
            labs(title = paste0("Movies streak (", iyear, ")"), x = "Month", y = "Day") +
            theme(
                axis.ticks = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_blank(),
                panel.grid = element_blank()
            )
        ggsave(file = paste0("RatingsCoeos", iyear, ".png"), plot = p, width = 8, height = 2, units = "in", dpi = 300)
    })

    dta <- do.call("rbind", lapply(ratings[, "Genres"], function (x) {
        out <- factor(unlist(strsplit(x, split = ", ")), levels = unlist(strsplit(x, split = ", ")))
        cbind.data.frame(
            Genres = levels(out),
            Weight = as.numeric(rev(out))/sum(as.numeric(out))
        )
    }))
    dta <- do.call("rbind", by(dta, dta[, "Genres"], function (idta) {
        cbind.data.frame(
            Genres = unique(idta[, "Genres"]),
            Score = sum(idta[, "Weight"])
        )
    }))
    dta <- dta[order(dta[, "Score"], decreasing = TRUE), ]
    dta[, "Genres"] <- factor(dta[, "Genres"], levels = dta[, "Genres"])
    pDta <- ggplot(data = dta, aes(x = factor(1), y = Score, fill = Genres)) +
        geom_bar(colour = "white", width = 1, stat = "identity") +
        coord_polar(theta = "y") +
        scale_fill_viridis(discrete = TRUE) +
        theme(
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank()
        )
    ggsave(file = "RatingsCoeos_Genres.png", plot = ggdraw(plot = pDta, theme_dark = theme_dark), width = 7.5, height = 6, units = "in", dpi = 300)


    dta <- do.call("rbind", lapply(seq_len(nrow(ratings)), function (irow) {
        out <- factor(unlist(strsplit(ratings[irow, "Genres"], split = ", ")), levels = unlist(strsplit(ratings[irow, "Genres"], split = ", ")))
        cbind.data.frame(
            Genres = levels(out),
            Weight = as.numeric(rev(out))/sum(as.numeric(out)),
            Rating = rep(ratings[irow, "Your.Rating"], length(levels(out)))
        )
    }))
    dta <- do.call("rbind", by(dta, dta[, "Genres"], function (idta) {
        cbind.data.frame(
            Genres = unique(idta[, "Genres"]),
            Rating = (idta[, "Weight"]%*%idta[, "Rating"])/sum(idta[, "Weight"]),
            Rating.avg = mean(idta[, "Rating"]),
            N = length(idta[, "Rating"])
        )
    }))
    dta[, "pN"] <- dta[, "N"]/sum(dta[, "N"])
    dta <- dta[order(dta[, "pN"], decreasing = TRUE), ]
    dta[, "Genres"] <- factor(dta[, "Genres"], levels = dta[, "Genres"])
    pDta <- ggplot(data = dta, aes(x = Genres, y = Rating, fill = N)) +
            geom_bar(width = 1, stat = "identity", colour    ="white") +
            scale_fill_viridis(discrete = FALSE, direction = 1) +
            scale_x_discrete(expand = c(0, 0)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
            geom_hline(yintercept = 5, colour = "white", linetype = 2) +
            geom_vline(xintercept = min(which(dta[, "pN"]<median(dta[, "pN"])))-0.5, colour = "white", linetype = 2) +
            labs(title = "Average Weighted Rating per Genre") +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
            )
    ggsave(file = "RatingsCoeos_GenresRating.png", plot = pDta, width = 7.5, height = 6, units = "in", dpi = 300)


    pDta <- ratings[, c("Your.Rating", "Runtime..mins.")] %>% 
        group_by(Your.Rating) %>% 
        summarise(Runtime = sum(Runtime..mins., na.rm = TRUE)/(60*24)) %>% 
        data.frame %>%
        ggplot(data = ., aes(x = factor(1), y = Runtime, fill = factor(Your.Rating))) +
            geom_bar(width = 1, stat = "identity") +
            geom_label(aes(
                    x = 1.5,
                    y = cumsum(rev(Runtime))-rev(Runtime)/2,
                    label = round(rev(Runtime), digits = 1)
            ), fill = "white", size = 3) +
            coord_polar(theta = "y") +
            scale_fill_viridis(name = "Rating", discrete = TRUE) +
            labs(title = "Days Spent Watching Movies per Rating") +
            theme(
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                panel.border = element_blank()
            )
    ggsave(file = "RatingsCoeos_Runtime.png", plot = ggdraw(plot = pDta, theme_dark = theme_dark), width = 7.5, height = 6, units = "in", dpi = 300)


###
    ggdta <- gather(data = ratings[, c("Date.Added", "Date", "Runtime..mins.", "Your.Rating", "IMDb.Rating")], key = Who, value = Rating, -c(Runtime..mins., Date, Date.Added))
    ggdta[, "Year"] <- year(ggdta[, "Date"])
    ggdta[, "Month"] <- month(ggdta[, "Date"])
    ggdta[, "Day"] <- day(ggdta[, "Date"])
    ggdta[, "wDay"] <- wday(ggdta[, "Date"], label = TRUE, abbr = FALSE)
    ggdta[, "wDay"] <- factor(ggdta[, "wDay"], levels = c(levels(ggdta[, "wDay"])[-1], levels(ggdta[, "wDay"])[1]))
    ggdta <- ggdta[order(ggdta[, "Date"]), ]
    ggdta[, "fDay"] <- as.factor(paste(ggdta[, "wDay"], ggdta[, "Day"]))

    
    ( ggplot(data = ggdta, aes(x = round(Rating, digits = 0), fill = Who)) +
        geom_density(aes(y = ..count../sum(..count..)*100), bw = 2, alpha = 0.75, colour = "white") +
        geom_bar(aes(y = ..count../sum(..count..)), width = 0.90, colour = "white", position = "dodge") +
        scale_x_continuous(name = "Rating", limits = c(0, 10), breaks = seq(0, 10, 2)) +
        scale_y_continuous(labels = percent)+
        scale_fill_viridis(discrete = TRUE) +
        labs(x = "Rating", y = "Proportion") +
        theme(legend.position = c(0, 1), legend.justification = c(-0.05, 1.05))
    ) %>%
    ggsave(file = "RatingsCoeos_IMDb.png", plot = ., width = 7.5, height = 6, units = "in", dpi = 300)
    
    # pDta <- subset(ggdta, Who=="You.rated" & Date>as.Date("2015-02-01")) %>% 
    #     group_by(Month, Year) %>% 
    #     summarise(Runtime = sum(Runtime..mins., na.rm = TRUE)) %>% 
    #     data.frame %>%
    #     ggplot(data = ., aes(x = , y = Runtime/60)) +
    #         geom_bar(stat = "identity", fill = viridis_pal()(1), colour = "white")
    
    