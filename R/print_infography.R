print_infography <- function(.x, .a, .b, .c, .d, .e, .g, .h, .i, gg_fontsize = params$gg_fontsize) {
  ggarrange(
    ggarrange(
      .b[[1]],
      ggarrange(
        .a[[1]],
        ggarrange(
          .i[[1]],
          ggarrange(.c[[1]], .d[[1]], ncol = 1, nrow = 2),
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
        color = ggplot2::theme_get()$text$colour, 
        face = "bold", 
        size = 16
      ),
      left = "",
      right = "", 
      bottom = text_grob(
        "© Mickaël 'Coeos' Canouil", 
        color = ggplot2::theme_get()$text$colour, 
        face = "bold",
        hjust = -8.08,
        size = 5
      )
    ) %>%
    as_ggplot()
}
