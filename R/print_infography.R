print_infography <- function(.x, .a, .b, .c, .d, .e, .g, .h, .i, gg_fontsize = params$gg_fontsize, base_family) {
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      .b[[1]],
      ggpubr::ggarrange(
        .a[[1]],
        ggpubr::ggarrange(
          .i[[1]],
          ggpubr::ggarrange(.c[[1]], .d[[1]], ncol = 1, nrow = 2),
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
    ggpubr::ggarrange(
      .e[[1]],
      .h[[1]] + 
        ggplot2::guides(fill = ggplot2::guide_legend(
          ncol = 1, 
          keyheight = ggplot2::unit(gg_fontsize*2/3, "pt"),
          keywidth = ggplot2::unit(gg_fontsize, "pt")
        )), 
      .g[[1]] + 
        ggplot2::guides(fill = ggplot2::guide_legend(
          ncol = 1, 
          keyheight = ggplot2::unit(gg_fontsize*1.2, "pt"),
          keywidth = ggplot2::unit(gg_fontsize*1.2, "pt")
        )), 
      ncol = 3,
      align = "v",
      widths = c(0.5, 0.25, 0.25)
    ),
    nrow = 2,
    heights = c(2/3, 1/3),
    align = "v"
  ) %>%
    gridExtra::arrangeGrob(
      top = ggpubr::text_grob(
        paste("Movies in", .x), 
        color = ggplot2::theme_get()$text$colour, 
        face = "bold", 
        size = 16,
        family = base_family
      ),
      left = "",
      right = "", 
      bottom = ggpubr::text_grob(
        "© Mickaël 'Coeos' Canouil", 
        color = ggplot2::theme_get()$text$colour, 
        face = "bold",
        # hjust = 0.5,
        size = 5,
        family = base_family
      )
    ) %>%
    ggpubr::as_ggplot()
}
