# modified from ggplot2 function add_margins()
add_margins <- function(grob, margin = NULL, margin_x = FALSE, margin_y = FALSE, debug = FALSE) {
  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }
  
  width <- grobWidth(grob)
  height <- grobHeight(grob)
  
  if (margin_x && margin_y) {
    widths <- unit.c(margin[4], width, margin[2])
    heights <- unit.c(margin[1], height, margin[3])
    
    vp <- viewport(
      layout = grid.layout(3, 3, heights = heights, widths = widths)
    )
    child_vp <- viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (margin_x) {
    widths <- unit.c(margin[4], width, margin[2])
    heights <- unit(1, "null")
    
    vp <- viewport(layout = grid.layout(1, 3, widths = widths))
    child_vp <- viewport(layout.pos.col = 2)
  } else if (margin_y) {
    widths <- unit(1, "null")
    heights <- unit.c(margin[1], height, margin[3])
    
    vp <- viewport(layout = grid.layout(3, 1, heights = heights))
    child_vp <- viewport(layout.pos.row = 2)
  } else {
    widths <- width
    heights <- height
    
    if (isTRUE(debug)) {
      bg <- rectGrob(gp = gpar(fill = "cornsilk"))
      g <- gTree(
        children = gList(bg, grob),
        widths = widths,
        heights = heights,
        cl = "titleGrob"
      )
    } else {
      g <- gTree(
        children = gList(grob),
        widths = widths,
        heights = heights,
        cl = "titleGrob"
      )
    }
    return(g)
  }
  
  if (isTRUE(debug)) {
    bg <- rectGrob(gp = gpar(fill = "cornsilk", col = NA))
    gTree(
      children = gList(bg, grob),
      vp = vpTree(vp, vpList(child_vp)),
      widths = widths,
      heights = heights,
      cl = "titleGrob"
    )
  }
  else {
    gTree(
      children = gList(grob),
      vp = vpTree(vp, vpList(child_vp)),
      widths = widths,
      heights = heights,
      cl = "titleGrob"
    )
  }
}
