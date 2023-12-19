test_that("visual test", {
  skip_if_not(packageVersion("ggplot2") >= "3.5.0")
  
  p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
    geom_point(key_glyph = "richtext") +
    scale_colour_discrete(
      labels = NULL,
      guide = guide_legend(override.aes = list(
        label = paste0(
          "<i><span style='color:black'>Iris</span>", 
          c("<br>setosa", " versicolor", "<br>virginica"), "</i>"
        ),
        size = 11 / .pt, hjust = c(1, 0.5, 0), angle = c(-45, 0, 45),
        label.colour = "blue"
      ))
    )
  expect_doppelganger("Rotated rich text keys", p)
})
