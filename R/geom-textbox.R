#' Draw boxes containing text
#'
#' Draw boxes of defined width and height containing word-wrapped text.
#' 
#' @inheritParams ggplot2::geom_text
#' @param width,height Unit values specifying the width and height of
#'   the text box. If `height = NULL` (the default), the height is chosen
#'   automatically to accommodate all the text.
#' @param minwidth,maxwidth,minheight,maxheight Unit values specifying
#'   the minimum and maximum values for `width` and `height`, respectively.
#'   If set to `NULL`, are not enforced.
#' @param box.padding Unit vector of length four specifying the padding
#'   inside the text box.
#' @param box.r Unit vector of length one specifying the radius of the
#'   box.
#' @examples
#' library(ggplot2)
#' 
#' df <- data.frame(
#'   label = rep("Lorem ipsum dolor **sit amet,** consectetur adipiscing elit,
#'     sed do *eiusmod tempor incididunt* ut labore et dolore magna
#'     aliqua.", 2),
#'   x = c(0, .6),
#'   y = c(1, .6),
#'   hjust = c(0, 0),
#'   vjust = c(1, 0),
#'   orientation = c("upright", "right-rotated"),
#'   color = c("black", "blue"),
#'   fill = c("cornsilk", "white")
#' )
#' 
#' ggplot(df) +
#'   aes(
#'     x, y, label = label, color = color, fill = fill,
#'     hjust = hjust, vjust = vjust,
#'     orientation = orientation
#'   ) +
#'   geom_textbox(width = unit(0.4, "npc")) +
#'   geom_point(color = "black", size = 2) +
#'   scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) +
#'   xlim(0, 1) + ylim(0, 1)
#' @export
geom_textbox <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         nudge_x = 0,
                         nudge_y = 0,
                         box.padding = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
                         box.r = unit(5.5, "pt"),
                         width = unit(2, "inch"), minwidth = NULL, maxwidth = NULL,
                         height = NULL, minheight = NULL, maxheight = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y` but not both.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextBox,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      box.padding = box.padding,
      box.r = box.r,
      width = width, minwidth = minwidth, maxwidth = maxwidth,
      height = height, minheight = minheight, maxheight = maxheight,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_richtext
#' @format NULL
#' @usage NULL
#' @export
GeomTextBox <- ggproto("GeomTextBox", Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
    colour = "black", fill = "white", size = 3.88, hjust = 0.5,
    vjust = 0.5, halign = 0, valign = 1, alpha = NA, family = "",
    fontface = 1, lineheight = 1.2, text.colour = NULL, box.colour = NULL,
    box.size = 0.25, orientation = "upright"
  ),

  draw_panel = function(data, panel_params, coord, 
                        box.padding = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
                        box.r = unit(5.5, "pt"),
                        width = unit(2, "inch"), minwidth = NULL, maxwidth = NULL,
                        height = NULL, minheight = NULL, maxheight = NULL,
                        na.rm = FALSE) {
    data <- coord$transform(data, panel_params)
    
    # split data frame into list of rows
    rows <- split(data, seq(nrow(data)))
    names(rows) <- NULL
    grobs <- mapply(
      make_textbox_grob,
      rows,
      list(box.padding),
      list(box.r),
      list(width),
      list(minwidth),
      list(maxwidth),
      list(height),
      list(minheight),
      list(maxheight),
      SIMPLIFY = FALSE
    )
    do.call(grobTree, grobs)
  },

  draw_key = draw_key_text
)

make_textbox_grob <- function(data, 
                              box.padding = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
                              box.r = unit(5.5, "pt"),
                              width = unit(2, "inch"), minwidth = NULL, maxwidth = NULL,
                              height = NULL, minheight = NULL, maxheight = NULL) {
  textbox_grob(
    data$label,
    data$x, data$y, default.units = "native",
    hjust = data$hjust, vjust = data$vjust,
    halign = data$halign, valign = data$valign,
    orientation = data$orientation,
    padding = box.padding,
    width = width, minwidth = minwidth, maxwidth = maxwidth,
    height = height, minheight = minheight, maxheight = maxheight,
    gp = gpar(
      col = scales::alpha(data$text.colour %||% data$colour, data$alpha),
      fontsize = data$size * .pt,
      fontfamily = data$family,
      fontface = data$fontface,
      lineheight = data$lineheight
    ),
    box_gp = gpar(
      col = scales::alpha(data$box.colour %||% data$colour, data$alpha),
      fill = scales::alpha(data$fill, data$alpha),
      lwd = data$box.size * .pt
    ),
    r = box.r
  )
}