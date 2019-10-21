#' Theme element that enables markdown text.
#' 
#' Theme element that enables markdown text.
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
#' @param debug Draw a debugging box around each label
#' @param inherit.blank See [`ggplot2::margin()`] for details.
#' @export
element_markdown <- function(family = NULL, face = NULL, colour = NULL, size = NULL,
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
    class = c("element_markdown", "element_text", "element")
  )
}

#' @export
element_grob.element_markdown <- function(element, label = "", x = NULL, y = NULL,
                                          family = NULL, face = NULL, colour = NULL, size = NULL,
                                          hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                                          margin = NULL, ...) {
  if (is.null(label))
    return(ggplot2::zeroGrob())

  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
  angle <- angle %||% element$angle %||% 0

  # We rotate the justifiation values to obtain the correct x and y reference point,
  # since hjust and vjust are applied relative to the rotated text frame in richtext_grob
  just <- rotate_just(angle, hj, vj)
  
  n <- max(length(x), length(y), 1)
  x <- x %||% unit(rep(just$hjust, n), "npc")
  y <- y %||% unit(rep(just$vjust, n), "npc")

  # The gp settings can override element_gp
  gp <- gpar(
    fontsize = size %||% element$size,
    col = colour %||% element$colour,
    fontfamily = family %||% element$family,
    fontface = face %||% element$face,
    lineheight = lineheight %||% element$lineheight
  )
  
  richtext_grob(
    label, x = x, y = y, hjust = hj, vjust = vj, rot = angle,
    padding = margin, gp = gp, debug = element$debug
  )
}


# Copied from ggplot2 code base
# https://github.com/tidyverse/ggplot2/blob/49d438cf9981f9f0624a27131775ecd3d5bb3455/R/margins.R#L294-L334
rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  if (0 <= angle & angle < 90) {
    hnew <- hjust
    vnew <- vjust
  } else if (90 <= angle & angle < 180) {
    hnew <- 1 - vjust
    vnew <- hjust
  } else if (180 <= angle & angle < 270) {
    hnew <- 1 - hjust
    vnew <- 1 - vjust
  } else if (270 <= angle & angle < 360) {
    hnew <- vjust
    vnew <- 1 - hjust
  }
  
  list(hjust = hnew, vjust = vnew)
}
