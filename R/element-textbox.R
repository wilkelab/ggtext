#' Theme element that enables markdown text in a box.
#' 
#' Theme element that enables markdown text in a box. Doesn't work yet.
#' 
#' @param family Font family
#' @param face Font face
#' @param colour,color Text color
#' @param size Font size
#' @param hjust Horizontal justification
#' @param vjust Vertical justification
#' @param angle Angle (in degrees)
#' @param lineheight Line height
#' @param margin Margins around the text. See [`ggplot2::margin()`] for 
#'   details.
#' @param debug Not implemented.
#' @param inherit.blank See [`ggplot2::margin()`] for details.
#' @export
element_textbox <- function(family = NULL, face = NULL, colour = NULL, size = NULL,
                             hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                             color = NULL, margin = NULL,
                             debug = FALSE, inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  structure(
    list(
      family = family, face = face, colour = colour,
      size = size, hjust = hjust, vjust = vjust, angle = angle,
      lineheight = lineheight, margin = margin, debug = debug,
      inherit.blank = inherit.blank),
    class = c("element_textbox", "element_text", "element")
  )
}

#' @export
element_grob.element_textbox <- function(element, label = "", x = NULL, y = NULL,
                                          family = NULL, face = NULL, colour = NULL, size = NULL,
                                          hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                                          margin = NULL, margin_x = FALSE, margin_y = FALSE, ...) {
  if (is.null(label))
    return(ggplot2::zeroGrob())

  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
  angle <- angle %||% element$angle %||% 0

  x <- x %||% hj
  if (!is.unit(x))
    x <- unit(x, "npc")
  y <- y %||% vj
  if (!is.unit(y))
    y <- unit(y, "npc")

  # The gp settings can override element_gp
  gp <- gpar(
    fontsize = size %||% element$size,
    col = colour %||% element$colour,
    fontfamily = family %||% element$family,
    fontface = face %||% element$face,
    lineheight = lineheight %||% element$lineheight
  )

  textbox_grob(
    label, x = x, y = y, hjust = hj, vjust = vj,
    padding = margin, gp = gp
  )
}
