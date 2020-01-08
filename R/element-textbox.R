#' Theme element that enables markdown text in a box.
#' 
#' Theme element that enables markdown text in a box.
#' 
#' @param family Font family
#' @param face Font face
#' @param size Font size (in pt)
#' @param colour,color Text color
#' @param fill Fill color of the enclosing box
#' @param box.colour,box.color Line color of the enclosing box (if different from the text color)
#' @param linetype Line type of the enclosing box (like `lty` in base R)
#' @param linewidth Line width of the enclosing box (like `lwd` in base R)
#' @param hjust Horizontal justification
#' @param vjust Vertical justification
#' @param box.hjust Horizontal justification
#' @param box.vjust Vertical justification
#' @param lineheight Line height
#' @param width,height Unit objects specifying the width and height
#'   of the textbox, as in [gridtext::textbox_grob()].
#' @param minwidth,minheight,maxwidth,maxheight Min and max values for width and height.
#'   Set to NULL to impose neither a minimum nor a maximum.
#' @param padding,margin Padding and margins around the text box.
#'   See [`gridtext::textbox_grob()`] for details.
#' @param r Unit value specifying the corner radius of the box 
#' @param orientation Orientation of the text box. See [`gridtext::textbox_grob()`] for details.
#' @param debug Not implemented.
#' @param inherit.blank See [`ggplot2::margin()`] for details.
#' @seealso [`gridtext::textbox_grob()`]
#' @export
element_textbox <- function(family = NULL, face = NULL, size = NULL, colour = NULL, fill = NULL,
                            box.colour = NULL, linetype = NULL, linewidth = NULL,
                            hjust = NULL, vjust = NULL, box.hjust = NULL, box.vjust = NULL, lineheight = NULL,
                            margin = NULL, padding = NULL, width = NULL, height = NULL, minwidth = NULL,
                            maxwidth = NULL, minheight = NULL, maxheight = NULL, r = NULL,
                            orientation = NULL, color = NULL, box.color = NULL,
                            debug = FALSE, inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  
  if (!is.null(box.color))
    box.colour <- box.color
  
  structure(
    list(
      family = family, face = face, size = size, colour = colour, fill = fill, box.colour = box.colour,
      linetype = linetype, linewidth = linewidth, 
      hjust = hjust, vjust = vjust, box.hjust = box.hjust,
      box.vjust = box.vjust, lineheight = lineheight, 
      margin = margin, padding = padding, width = width, height = height, minwidth = minwidth,
      maxwidth = maxwidth, minheight = minheight, maxheight = maxheight,
      r = r, orientation = orientation,
      debug = debug, inherit.blank = inherit.blank),
    class = c("element_textbox", "element_text", "element")
  )
}

#' @export
element_grob.element_textbox <- function(element, label = "", x = NULL, y = NULL,
                                         family = NULL, face = NULL, colour = NULL, size = NULL,
                                         hjust = NULL, vjust = NULL, lineheight = NULL,
                                         margin = NULL, ...) {
  if (is.null(label))
    return(ggplot2::zeroGrob())

  hj <- hjust %||% element$hjust
  vj <- vjust %||% element$vjust
  box_hj <- element$box.hjust %||% hj
  box_vj <- element$box.vjust %||% vj
  padding <- element$padding %||% ggplot2::margin(0, 0, 0, 0)
  margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
  orientation <- element$orientation %||% "upright"
  r <- element$r %||% unit(0, "pt")
  
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
    lwd = element$linewidth %||% 1
  )
  
  textbox_grob(
    label, x = x, y = y, hjust = hj, vjust = vj, box_hjust = box_hj, box_vjust = box_vj,
    width = element$width, height = element$height,
    minwidth = element$minwidth, minheight = element$minheight,
    maxwidth = element$maxwidth, maxheight = element$maxheight,
    margin = margin, padding = padding, r = r,
    orientation = orientation, 
    gp = gp, box_gp = box_gp
  )
}
