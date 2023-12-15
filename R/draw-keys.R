#' Key glyph for rich text
#' 
#' This is a function for rendering the key in the rich text style when a geom 
#' needs to be displayed in a legend. It is designed to be provided to a layer's
#' `key_glyph` argument, either as a function or as `key_glyph = "richtext"`.
#'
#' @inheritParams ggplot2::draw_key
#'
#' @return A [`richtext_grob`][gridtext::richtext_grob] that represents
#'   formatted text.
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
#'   geom_richtext(aes(colour = factor(cyl)), key_glyph = "richtext")
draw_key_richtext <- function(data, params, size) {
  
  # Set text justification
  data$hjust <- data$hjust %||% 0.5
  data$vjust <- data$vjust %||% 0.5
  data$angle <- data$angle %||% 0
  just <- rotate_just(data$angle, data$hjust, data$vjust)
  
  # Populate graphical parameters for text
  text_gp <- gpar(
    fontsize = (data$size %||% 3.88) * .pt,
    fontfamily = data$family %||% "",
    fontface = data$face %||% 1,
    col = scales::alpha(
      data$text.colour %||% data$colour %||% "black", 
      data$alpha %||% NA
    ),
    lineheight = data$lineheight %||% 1.2
  )
  
  # Populate graphical parameters for text box
  box_gp <- gpar(
    col = scales::alpha(
      data$label.colour %||% data$colour %||% "black",
      data$alpha %||% NA
    ),
    fill = scales::alpha(data$fill %||% "white", data$alpha %||% NA),
    lwd = (data$label.size %||% 0.25) * .pt
  )
  
  grob <- richtext_grob(
    text = data[["label"]] %||% "a", # prevent partial matching
    x = unit(just$hjust, "npc"),
    y = unit(just$vjust, "npc"),
    rot = data$angle,
    hjust = just$hjust,
    vjust = just$vjust,
    gp = text_gp, 
    box_gp = box_gp,
    # Defaults for unit input are the same as `geom_richtext()` formals
    r = params$label.r %||% unit(0.15, "lines"),
    padding = params$label.padding %||% 
      unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
    margin = params$label.margin %||%
      unit(c(0, 0, 0, 0), "lines"),
  )

  # Key drawing functions deal with 1 key at a time, so we can extract the
  # box's (relative) coordinates from the first child-grob.
  # The units are given in points
  x <- range(grob$children[[1]]$xext)
  y <- range(grob$children[[1]]$yext)

  # Calculate offsets that account for textbox size
  xoffset <- x[1] * (1 - just$hjust) + x[2] * just$hjust
  yoffset <- y[1] * (1 - just$vjust) + y[2] * just$vjust
  
  # We apply offsets to the grob's viewport so that textbox is remains within 
  # the bounds of the key area
  grob <- editGrob(
    grob,
    vp = viewport(
      x = unit(0.5, "npc") - unit(xoffset, "pt"),
      y = unit(0.5, "npc") - unit(yoffset, "pt")
    )
  )
  
  # Calculate size in cm. 
  # 'x * .pt' converts mm to pt, so 'x / .pt' converts pt to mm
  # This circumvents `convertWidth(grobWidth(grob), "cm", valueOnly = TRUE)`
  attr(grob, "width")  <- diff(x) / (10 * .pt)
  attr(grob, "height") <- diff(y) / (10 * .pt)
  grob
}