#' Draw boxes containing text
#'
#' Draw boxes of defined width and height containing word-wrapped text.
#' 
#' @examples
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
    colour = "black", fill = "white", size = 3.88, hjust = 0,
    vjust = 1, box.hjust = NULL, box.vjust = NULL, alpha = NA, family = "",
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
    box_hjust = data$box.hjust %||% data$hjust,
    box_vjust = data$box.vjust %||% data$vjust,
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