#' theme_black
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @export
#' @import ggplot2
theme_black <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  half_line <- base_size / 2
  bc <- c("grey20", "grey50", "white")
  ggplot2::theme(
    line = ggplot2::element_line(
      colour = bc[3],
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = bc[1],
      colour = bc[3],
      size = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = bc[3],
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    title = NULL,
    aspect.ratio = NULL,

    axis.title = NULL,
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line), vjust = 1),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line), vjust = 0),
    axis.title.x.bottom = NULL,
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = half_line), vjust = 1),
    axis.title.y.left = NULL,
    axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line), vjust = 0),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = bc[3]),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.x.bottom = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.left = NULL,
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = bc[3]),
    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = NULL,
    axis.line.x.bottom = NULL,
    axis.line.y = NULL,
    axis.line.y.left = NULL,
    axis.line.y.right = NULL,

    legend.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),

    panel.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = bc[3], size = 0.5, linetype = "solid"),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.grid = ggplot2::element_line(colour = bc[2]),
    panel.grid.major = ggplot2::element_line(colour = bc[2]),
    panel.grid.minor = ggplot2::element_line(colour = bc[2], size = ggplot2::rel(0.5)),
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.ontop = FALSE,

    plot.background = ggplot2::element_rect(colour = bc[1]),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      hjust = 1, vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),

    strip.background = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    strip.background.x = NULL,
    strip.background.y = NULL,
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.text = ggplot2::element_text(
      colour = bc[3],
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),

    complete = TRUE
  )
}


#' @rdname theme_black
#' @export
theme_black_md <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  half_line <- base_size / 2
  bc <- c("grey20", "grey50", "white")
  theme_black(base_size, base_family, base_line_size, base_rect_size) +
    ggplot2::theme(
      # text = ggtext::element_markdown(
      #   family = base_family,
      #   face = "plain",
      #   colour = bc[3],
      #   size = base_size,
      #   lineheight = 0.9,
      #   hjust = 0.5,
      #   vjust = 0.5,
      #   angle = 0,
      #   margin = ggplot2::margin(),
      #   debug = FALSE
      # ),
      axis.title.x = ggtext::element_markdown(margin = ggplot2::margin(t = half_line), vjust = 1),
      axis.title.x.top = ggtext::element_markdown(margin = ggplot2::margin(b = half_line), vjust = 0),
      axis.title.y = ggtext::element_markdown(angle = 90, margin = ggplot2::margin(r = half_line), vjust = 1),
      axis.title.y.right = ggtext::element_markdown(angle = -90, margin = ggplot2::margin(l = half_line), vjust = 0),
      axis.text = ggtext::element_markdown(size = ggplot2::rel(0.8), colour = bc[3]),
      axis.text.x = ggtext::element_markdown(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
      axis.text.x.top = ggtext::element_markdown(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
      axis.text.y = ggtext::element_markdown(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
      axis.text.y.right = ggtext::element_markdown(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),

      legend.text = ggtext::element_markdown(size = ggplot2::rel(0.8)),
      legend.title = ggtext::element_markdown(hjust = 0),

      plot.title = ggtext::element_markdown(
        size = ggplot2::rel(1.2),
        face = "bold",
        hjust = 0,
        vjust = 1,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.subtitle = ggtext::element_markdown(
        hjust = 0,
        vjust = 1,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.caption = ggtext::element_markdown(
        size = ggplot2::rel(0.8),
        hjust = 1, vjust = 1,
        margin = ggplot2::margin(t = half_line)
      ),
      plot.tag = ggtext::element_markdown(size = ggplot2::rel(1.2), hjust = 0.5, vjust = 0.5),

      strip.text = ggtext::element_markdown(
        colour = bc[3],
        size = ggplot2::rel(0.8),
        margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
      ),
      strip.text.y = ggtext::element_markdown(angle = -90)
    )
}


#' Explicitly draw plot
#'
#' Generally, you do not need to print or plot a ggplot2 plot explicitly: the
#' default top-level print method will do it for you. You will, however, need
#' to call `print()` explicitly if you want to draw a plot inside a
#' function or for loop.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @return Invisibly returns the result of [ggplot_build()], which
#'   is a list with components that contain the plot itself, the data,
#'   information about the scales, panels etc.
#' @export
#' @method print ggplot
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  bg_fill <- ggplot2::calc_element("plot.background", ggplot2:::plot_theme(x))$fill

  ggplot2::set_last_plot(x)
  if (newpage) {
    grid::grid.newpage()
  }
  grid::grid.rect(gp = grid::gpar(fill = bg_fill, col = bg_fill))
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )
  data <- ggplot2::ggplot_build(x)
  gtable <- ggplot2::ggplot_gtable(data)
  if (is.null(vp)) {
    grid::grid.draw(gtable)
  } else {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
    }
    grid::grid.draw(gtable)
    grid::upViewport()
  }

  invisible(x)
}

#' @rdname print.ggplot
#' @method plot ggplot
#' @export
plot.ggplot <- print.ggplot

#' ggsave
#'
#' @inheritParams ggplot2::ggsave
#' @export
ggsave <- function(
  filename,
  plot = ggplot2::last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE,
  ...
) {
  bg_fill <- ggplot2::calc_element("plot.background", ggplot2:::plot_theme(plot))$fill
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = device,
    path = path,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    limitsize = limitsize,
    bg = bg_fill,
    ...
  )
}

#' compute_brightness
#'
#' @param colour vector of any of the three kinds of R color specifications,
#'     *i.e.*, either a color name (as listed by colors()),
#'     a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb).
#'
#' @keywords internal
compute_brightness <- function(colour) {
  ((sum(range(grDevices::col2rgb(colour)))) * 100 * 0.5) / 255
}


#' dark_mode
#'
#' @param .theme a theme (a list of theme elements)
#' @keywords internal
dark_mode <- function(.theme) {
  stopifnot(is.theme(.theme))
  geom_names <- utils::apropos("^Geom", ignore.case = FALSE)
  geoms <- list()
  namespaces <- loadedNamespaces()
  for (namespace in namespaces) {
    geoms_in_namespace <- mget(
      x = geom_names,
      envir = asNamespace(namespace),
      ifnotfound = list(NULL)
    )
    for (geom_name in geom_names) {
      if (ggplot2::is.ggproto(geoms_in_namespace[[geom_name]])) {
        geoms[[geom_name]] <- geoms_in_namespace[[geom_name]]
      }
    }
  }
  pick_colour <- c("white", "black")[(compute_brightness(.theme$plot.background$colour) > 50) + 1]
  for (geom in geoms) {
    stopifnot(ggplot2::is.ggproto(geom))
    if (!is.null(geom$default_aes$fill) && !is.na(geom$default_aes$fill)) {
      geom$default_aes$fill <- pick_colour
    }
    if (!is.null(geom$default_aes$colour) && !is.na(geom$default_aes$colour)) {
      geom$default_aes$colour <- pick_colour
    }
    if (inherits(geom, "GeomBoxplot") | inherits(geom, "GeomLabel")) {
      geom$default_aes$fill <- .theme$plot.background$colour
    }
  }

  invisible(.theme)
}
