ttheme <- function(
  base_style = "default", 
  base_size = 11, 
  base_colour = "black", 
  base_family = "",
  padding = unit(c(4, 4), "mm"), 
  colnames.style = colnames_style(size = base_size),
  rownames.style = rownames_style(size = base_size), 
  tbody.style = tbody_style(size = base_size)
) {
  style <- ggpubr:::tstyle(base_style, size = base_size)
  if (!is.null(style)) {
    colnames.style <- style$colnames.style
    rownames.style <- style$rownames.style
    tbody.style <- style$tbody.style
  }
  .ttheme <- gridExtra::ttheme_default(
    base_size = base_size,
    base_colour = base_colour,
    padding = padding,
    base_family = base_family
  )
  .ttheme$colhead <- do.call(ggpubr:::.add_item, c(
    list(.list = .ttheme$colhead),
    colnames.style
  ))
  .ttheme$colhead$fg_params$fontfamily <- base_family
  .ttheme$rowhead <- do.call(ggpubr:::.add_item, c(
    list(.list = .ttheme$rowhead),
    rownames.style
  ))
  .ttheme$rowhead$fg_params$fontfamily <- base_family
  .ttheme$core <- do.call(ggpubr:::.add_item, c(
    list(.list = .ttheme$core),
    tbody.style
  ))
  .ttheme$core$fg_params$fontfamily <- base_family
  attr(.ttheme, "style") <- base_style
  .ttheme
}