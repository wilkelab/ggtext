#' Theme element that enables markdown text in a box.
#' 
#' The theme elements `element_textbox()` and `element_textbox_simple()` enable Markdown text in a box, with
#' word wrap. Both functions implement exactly the same functionality; they only differ in the default values
#' for the various element values. `element_textbox()` sets all values that are not specified to `NULL`, as is
#' the usual practice in ggplot2 themes. These missing values are usually completed by inheritance from 
#' parent theme elements. By contrast, `element_textbox_simple()` provides meaningful default values for many of
#' the values that are not usually defined in ggplot2 themes. This makes it simpler to use a textbox element
#' in the context of an existing theme.
#' 
#' @param family Font family
#' @param face Font face
#' @param size Font size (in pt)
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
#' @param width,height Unit objects specifying the width and height
#'   of the textbox, as in [gridtext::textbox_grob()].
#' @param minwidth,minheight,maxwidth,maxheight Min and max values for width and height.
#'   Set to NULL to impose neither a minimum nor a maximum.
#' @param padding,margin Padding and margins around the text box.
#'   See [gridtext::textbox_grob()] for details.
#' @param r Unit value specifying the corner radius of the box 
#' @param orientation Orientation of the text box. See [gridtext::textbox_grob()] for details.
#' @param debug Not implemented.
#' @param inherit.blank See [ggplot2::margin()] for details.
#' @seealso [gridtext::textbox_grob()], [element_markdown()]
#' @examples
#' library(ggplot2)
#' 
#' ggplot(mtcars, aes(disp, mpg)) + 
#' geom_point() +
#'   labs(
#'     title =
#'     "<b><span style = 'font-size:13pt'>Fuel economy vs. engine displacement</span></b><br>
#'     Lorem ipsum *dolor sit amet,* consectetur adipiscing elit, **sed do eiusmod tempor
#'     incididunt** ut labore et dolore magna aliqua. <span style = 'color:red;'>Ut enim ad
#'     minim veniam,</span> quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
#'     commodo consequat.",
#'     x = "displacement (in<sup>3</sup>)",
#'     y = "Miles per gallon (mpg)<br><span style = 'font-size:8pt'>A measure of
#'     the car's fuel efficiency.</span>"
#'   ) +
#'   theme(
#'     plot.title.position = "plot",
#'     plot.title = element_textbox_simple(
#'       size = 10,
#'       padding = margin(5.5, 5.5, 5.5, 5.5),
#'       margin = margin(0, 0, 5.5, 0),
#'       fill = "cornsilk"
#'     ),
#'     axis.title.x = element_textbox_simple(
#'       width = NULL,
#'       padding = margin(4, 4, 4, 4),
#'       margin = margin(4, 0, 0, 0),
#'       linetype = 1,
#'       r = grid::unit(8, "pt"),
#'       fill = "azure1"
#'     ),
#'     axis.title.y = element_textbox_simple(
#'       hjust = 0,
#'       orientation = "left-rotated",
#'       minwidth = unit(1, "in"),
#'       maxwidth = unit(2, "in"),
#'       padding = margin(4, 4, 2, 4),
#'       margin = margin(0, 0, 2, 0),
#'       fill = "lightsteelblue1"
#'     )
#'   )
#' @export
element_textbox <- function(family = NULL, face = NULL, size = NULL, colour = NULL, fill = NULL,
                            box.colour = NULL, linetype = NULL, linewidth = NULL,
                            hjust = NULL, vjust = NULL, halign = NULL, valign = NULL, lineheight = NULL,
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
      hjust = hjust, vjust = vjust, halign = halign, valign = valign, lineheight = lineheight, 
      margin = margin, padding = padding, width = width, height = height, minwidth = minwidth,
      maxwidth = maxwidth, minheight = minheight, maxheight = maxheight,
      r = r, orientation = orientation,
      debug = debug, inherit.blank = inherit.blank),
    class = c("element_textbox", "element_text", "element")
  )
}

#' @rdname element_textbox
#' @export
element_textbox_simple <- function(family = NULL, face = NULL, size = NULL, colour = NULL, fill = NA,
                            box.colour = NULL, linetype = 0, linewidth = 0.5,
                            hjust = 0.5, vjust = 0.5, halign = 0, valign = 1, lineheight = 1.1,
                            margin = ggplot2::margin(0, 0, 0, 0), padding = ggplot2::margin(0, 0, 0, 0),
                            width = grid::unit(1, "npc"), height = NULL, minwidth = NULL,
                            maxwidth = NULL, minheight = NULL, maxheight = NULL, r = grid::unit(0, "pt"),
                            orientation = "upright", color = NULL, box.color = NULL,
                            debug = FALSE, inherit.blank = FALSE) {
  element_textbox(
    family = family, face = face, size = size, colour = colour, fill = fill,
    box.colour = box.colour, linetype = linetype, linewidth = linewidth, hjust = hjust, vjust = vjust,
    halign = halign, valign = valign, lineheight = lineheight, margin = margin, padding = padding,
    width = width, height = height, minwidth = minwidth, maxwidth = maxwidth, minheight = minheight,
    maxheight = maxheight, r = r, orientation = orientation, color = color, box.color = box.color
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
  halign <- element$halign %||% 0
  valign <- element$valign %||% 1
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
    lwd = (element$linewidth %||% 0.5)*ggplot2::.pt
  )
  
  textbox_grob(
    label, x = x, y = y, hjust = hj, vjust = vj, halign = halign, valign = valign,
    width = element$width, height = element$height,
    minwidth = element$minwidth, minheight = element$minheight,
    maxwidth = element$maxwidth, maxheight = element$maxheight,
    margin = margin, padding = padding, r = r,
    orientation = orientation, 
    gp = gp, box_gp = box_gp
  )
}
