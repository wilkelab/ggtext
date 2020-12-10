context("element textbox")

test_that("visual tests", {
  text <- "*Lorem ipsum dolor sit amet,* consectetur adipiscing
elit. **Quisque tincidunt** eget arcu in pulvinar. Morbi varius leo
vel consectetur luctus. **Morbi facilisis justo non fringilla.**
Vivamus sagittis sem felis, vel lobortis risus mattis eget. Nam
quis imperdiet felis, in convallis elit."
  
  base <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()
  
  p <- base +
    labs(subtitle = text) +
    theme(
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(
        size = 11, lineheight = 1.2, padding = margin(0, 0, 5, 0)
      )
    )
  
  expect_doppelganger("Simple textbox as plot title", p)
  
  p <- base +
    labs(subtitle = text) +
    theme(
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(
        size = 11, lineheight = 1.2,
        linetype = 1, # turn on border
        box.color = "#748696", # border color
        fill = "#F0F7FF", # background fill color
        r = grid::unit(3, "pt"), # radius for rounded corners
        padding = margin(5, 5, 5, 5), # padding around text inside the box
        margin = margin(0, 0, 10, 0) # margin outside the box
      )
    )
  
  expect_doppelganger("Plot title with styling", p)
  
  p <- base +
    labs(subtitle = text) +
    theme(
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(
        size = 11, lineheight = 1.2,
        width = grid::unit(4, "in"), # fixed width
        hjust = 1, # alignment of box relative to plot
        linetype = 1, # turn on border
        box.color = "#748696", # border color
        fill = "#F0F7FF", # background fill color
        r = grid::unit(3, "pt"), # radius for rounded corners
        padding = margin(5, 5, 5, 5), # padding around text inside the box
        margin = margin(0, 0, 10, 0) # margin outside the box
      )
    )
  
  expect_doppelganger("Plot title with fixed width", p)
  
  p <- base +
    labs(y = text) +
    theme(
      axis.title.y = element_textbox_simple(
        size = 11, lineheight = 1.2,
        orientation = "left-rotated",
        width = grid::unit(2.5, "in"),
        hjust = 0, fill = "#F0F7FF",  
        padding = margin(5, 5, 5, 5),
        margin = margin(0, 0, 10, 0)
      )
    )
  
  expect_doppelganger("Rotated box as y axis title", p)
})
