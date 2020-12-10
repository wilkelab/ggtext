context("geom richtext")

test_that("visual tests", {
  df <- data.frame(
    x = 0.5,
    y = 0.5,
    label = "The **quick** *brown* fox.",
    angle = c(0, 45, 90, 135),
    label.colour = c("#858E4E", "#24988F", "#8C82B8", "#B97681"),
    fill = c("#E6EBD1", "#CDEFE9", "#E8E6FC", "#FEE1E5"),
    label.size = c(0.1, 0.25, 0.5, 1)
  )
  
  p <- ggplot() +
    geom_richtext(
      data = df,
      aes(
        x, y, label = label, label.colour = label.colour, fill = fill,
        label.size = label.size, angle = angle
      ),
      label.margin = grid::unit(rep(0.25, 4), "in"),
      hjust = 0, vjust = 0.5
    ) +
    xlim(0, 1) + ylim(0, 1) +
    scale_discrete_identity(aesthetics = c("orientation", "label.colour", "fill"))
  
  expect_doppelganger("Rotated labels w/ colors", p)
})
