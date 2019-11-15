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
#' @param rotate_margins Should margins get rotated in frame with rotated text?
#'   If `TRUE`, the margins are applied relative to the text direction. If `FALSE`,
#'   the margins are applied relative to the plot direction, i.e., the top margin,
#'   for example, is always placed above the text label, regardless of the direction
#'   in which the text runs. The default is `FALSE`, which mimics the behavior of 
#'   `element_text()`.
#' @param debug Draw a debugging box around each label
#' @param inherit.blank See [`ggplot2::margin()`] for details.
#' @export
element_markdown <- function(family = NULL, face = NULL, colour = NULL, size = NULL,
                             hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                             color = NULL, margin = NULL, rotate_margins = FALSE,
                             debug = FALSE, inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  structure(
    list(
      family = family, face = face, colour = colour,
      size = size, hjust = hjust, vjust = vjust, angle = angle,
      lineheight = lineheight, margin = margin, rotate_margins = rotate_margins,
      debug = debug, inherit.blank = inherit.blank),
    class = c("element_markdown", "element_text", "element")
  )
}

#' @export
element_grob.element_markdown <- function(element, label = "", x = NULL, y = NULL,
                                          family = NULL, face = NULL, colour = NULL, size = NULL,
                                          hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                                          margin = NULL, margin_x = FALSE, margin_y = FALSE, ...) {
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
  
  
  mrg <- fixup_margins(element$rotate_margins, margin, angle)
  
  if (isTRUE(mrg$native_margins)) {
    richtext_grob(
      label, x = x, y = y, hjust = hj, vjust = vj, rot = angle,
      padding = mrg$margin, gp = gp, debug = element$debug
    )
  } else {
    grob <- richtext_grob(
      label, x = x, y = y, hjust = hj, vjust = vj, rot = angle,
      gp = gp, debug = element$debug
    )
    add_margins(grob, margin, margin_x, margin_y, debug = element$debug)
  }
}


# Modeled after ggplot2 function:
# https://github.com/tidyverse/ggplot2/blob/49d438cf9981f9f0624a27131775ecd3d5bb3455/R/margins.R#L294-L334
# modified here to work with vectorized input
rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  
  hnew <- ifelse(
    0 <= angle & angle < 90,
    hjust,
    ifelse(
      90 <= angle & angle < 180,
      1 - vjust,
      ifelse(
        180 <= angle & angle < 270,
        1 - hjust,
        vjust
      )
    )
  )
  
  vnew <- ifelse(
    0 <= angle & angle < 90,
    vjust,
    ifelse(
      90 <= angle & angle < 180,
      hjust,
      ifelse(
        180 <= angle & angle < 270,
        1 - vjust,
        1 - hjust
      )
    )
  )
  
  list(hjust = hnew, vjust = vnew)
}


fixup_margins <- function(rotate_margins, margin, angle) {
  if (isTRUE(rotate_margins)) {
    return(list(native_margins = TRUE, margin = margin))
  }
  
  angle <- round(angle) %% 360
  
  # if we're given more than one angle or angles corresponding to anything
  # other than horizontal or vertical positions, we cannot use native margins
  if (length(unique(angle)) > 1 || any(!angle %in% c(0, 90, 180, 270))) {
    return(list(native_margins = FALSE, margin = NULL))
  } else {
    angle <- angle[1]
  }
  
  if (angle == 0) {
    return(list(native_margins = TRUE, margin = margin))
  }
  
  if (angle == 90) {
    return(list(native_margins = TRUE, margin = margin[c(4, 1, 2, 3)]))
  }
  
  if (angle == 270) {
    return(list(native_margins = TRUE, margin = margin[c(2, 3, 4, 1)]))
  }
  
  # the only case remaining is angle == 180
  return(list(native_margins = TRUE, margin = margin[c(3, 4, 1, 2)]))
}
