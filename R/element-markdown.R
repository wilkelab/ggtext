#' Theme element that enables markdown text.
#' 
#' Theme element that enables markdown text.
#' 
#' @param family Font family
#' @param face Font face
#' @param size Font size
#' @param colour,color Text color
#' @param fill Fill color of the enclosing box
#' @param box.colour,box.color Line color of the enclosing box (if different from the text color)
#' @param linetype Line type of the enclosing box (like `lty` in base R)
#' @param linewidth Line width of the enclosing box (measured in mm, just like `size` in
#'   [ggplot2::element_line()]).
#' @param hjust Horizontal justification
#' @param vjust Vertical justification
#' @param halign Horizontal justification
#' @param valign Vertical justification
#' @param lineheight Line height
#' @param padding,margin Padding and margins around the text box.
#'   See [gridtext::richtext_grob()] for details.
#' @param r Unit value specifying the corner radius of the box 
#' @param angle Angle (in degrees)
#' @param align_widths,align_heights Should multiple elements be aligned by their
#'   widths or height? See [gridtext::richtext_grob()] for details.
#' @param rotate_margins Should margins get rotated in frame with rotated text?
#'   If `TRUE`, the margins are applied relative to the text direction. If `FALSE`,
#'   the margins are applied relative to the plot direction, i.e., the top margin,
#'   for example, is always placed above the text label, regardless of the direction
#'   in which the text runs. The default is `FALSE`, which mimics the behavior of 
#'   `element_text()`.
#' @param debug Draw a debugging box around each label
#' @param inherit.blank See [ggplot2::margin()] for details.
#' @return A ggplot2 theme element that can be used inside a [ggplot2::theme()]
#'   call.
#' @seealso [gridtext::richtext_grob()], [element_textbox()], [geom_richtext()]
#' @export
element_markdown <- function(family = NULL, face = NULL, size = NULL, colour = NULL, fill = NULL,
                             box.colour = NULL, linetype = NULL, linewidth = NULL,
                             hjust = NULL, vjust = NULL, halign = NULL, valign = NULL, angle = NULL,
                             lineheight = NULL, margin = NULL, padding = NULL, r = NULL, 
                             color = NULL, box.color = NULL, align_widths = NULL,
                             align_heights = NULL, rotate_margins = NULL,
                             debug = FALSE, inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  
  if (!is.null(box.color))
    box.colour <- box.color
  
  structure(
    list(
      family = family, face = face, size = size, colour = colour, fill = fill,
      box.colour = box.colour, linetype = linetype, linewidth = linewidth, 
      hjust = hjust, vjust = vjust, halign = halign, valign = valign, angle = angle,
      lineheight = lineheight, margin = margin, padding = padding, r = r, 
      align_widths = align_widths, align_heights = align_heights, 
      rotate_margins = rotate_margins,
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

  hj <- hjust %||% element$hjust
  vj <- vjust %||% element$vjust
  halign <- element$halign %||% hj
  valign <- element$valign %||% vj
  padding <- element$padding %||% ggplot2::margin(0, 0, 0, 0)
  margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
  angle <- angle %||% element$angle %||% 0
  r <- element$r %||% unit(0, "pt")
  align_widths <- isTRUE(element$align_widths)
  align_heights <- isTRUE(element$align_heights)
  
  # We rotate the justifiation values to obtain the correct x and y reference point,
  # since box_hjust and box_vjust are applied relative to the rotated text frame in richtext_grob
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
  
  box_gp <- gpar(
    col = element$box.colour %||% gp$col,
    fill = element$fill %||% NA,
    lty = element$linetype %||% 0,
    lwd = (element$linewidth %||% 0.5)*ggplot2::.pt
  )
  
  mrg <- fixup_margins(element$rotate_margins, margin, angle)
  
  if (isTRUE(mrg$native_margins)) {
    richtext_grob(
      label, x = x, y = y, hjust = hj, vjust = vj, halign = halign, valign = valign,
      rot = angle, padding = padding, margin = mrg$margin, r = r, 
      align_widths = align_widths, align_heights = align_heights,
      gp = gp, box_gp = box_gp,
      debug = element$debug
    )
  } else {
    grob <- richtext_grob(
      label, x = x, y = y, hjust = hj, vjust = vj, halign = halign, valign = valign,
      rot = angle, padding = padding, margin = unit(c(0, 0, 0, 0), "pt"), r = r,
      align_widths = align_widths, align_heights = align_heights,
      gp = gp, box_gp = box_gp,
      debug = element$debug
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
