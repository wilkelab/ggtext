context("geom textbox")

test_that("visual tests", {
  df <- data.frame(
    x = 0.5,
    y = 0.5,
    label = "The quick brown fox **jumps over** the *lazy* dog.",
    orientation = c("upright", "left-rotated", "inverted", "right-rotated"),
    box.colour = c("#858E4E", "#24988F", "#8C82B8", "#B97681"),
    fill = c("#E6EBD1", "#CDEFE9", "#E8E6FC", "#FEE1E5"),
    halign = c(0, 0.5, 1, 0.5),
    valign = c(1, 0.5, 0.5, 0),
    box.size = c(0.1, 0.25, 0.5, 1)
  )
  
  p <- ggplot() +
    geom_textbox(
      data = df,
      aes(
        x, y, label = label, box.colour = box.colour, fill = fill,
        halign = halign, valign = valign, box.size = box.size,
        orientation = orientation
      ),
      width = grid::unit(1.5, "in"),
      height = grid::unit(1.5, "in"),
      box.margin = grid::unit(rep(0.25, 4), "in"),
      hjust = 0, vjust = 1
    ) +
    xlim(0, 1) + ylim(0, 1) +
    scale_discrete_identity(aesthetics = c("orientation", "box.colour", "fill"))
  
  vdiffr::expect_doppelganger("Rotated boxes w/ colors and alignments", p)
})
