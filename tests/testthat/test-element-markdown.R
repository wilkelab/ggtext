context("element markdown")

test_that("ggplot2 margin simulation", {
  # ggplot2 margins are simulated with reshuffled
  # richtext_grob margins for horizontal and vertical
  # text, but not for other angles
  el <- element_markdown(
    size = 12, family = "", face = "plain", colour = "black",
    hjust = 0.5, vjust = 0.5, lineheight = 0,
    margin = margin(10, 10, 10, 10)
  )
  g <- element_grob(
    el, "test", angle = 0, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  g <- element_grob(
    el, "test", angle = 90, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  g <- element_grob(
    el, "test", angle = 180, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  g <- element_grob(
    el, "test", angle = 270, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  g <- element_grob(
    el, "test", angle = 45, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "titleGrob")
  
  # when ggplot2 margins are turned off, result is
  # always a richtext_grob
  el <- element_markdown(
    size = 12, family = "", face = "plain", colour = "black",
    hjust = 0.5, vjust = 0.5, lineheight = 0,
    margin = margin(10, 10, 10, 10),
    rotate_margins = TRUE
  )
  g <- element_grob(
    el, "test", angle = 0, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  g <- element_grob(
    el, "test", angle = 90, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  g <- element_grob(
    el, "test", angle = 180, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  g <- element_grob(
    el, "test", angle = 270, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  
  g <- element_grob(
    el, "test", angle = 45, x = 0.5, y = 0.5,
    margin_x = TRUE, margin_y = TRUE
  )
  expect_s3_class(g, "richtext_grob")
  
})

test_that("visual tests", {
  
  df <- data.frame(x = seq(50, 450, by = 50), y = 9:1)
  
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    xlab("really really long x label") +
    ylab("long y label")
  
  debug <- FALSE
  
  p1 <- p +
    theme(
      axis.text.x = element_markdown(
        margin = margin(10, 0, 20, 0),
        debug = debug
      ),
      axis.title.y = element_markdown(
        margin = margin(0, 10, 0, 20),
        debug = debug
      )
    )
  
  p2 <- p +
    theme(
      axis.text.x = element_text(
        margin = margin(10, 0, 20, 0),
        debug = debug
      ),
      axis.title.y = element_text(
        margin = margin(0, 10, 0, 20),
        debug = debug
      )
    )
  
  p3 <- p +
    theme(
      axis.text.x = element_markdown(
        hjust = 1,
        margin = margin(10, 0, 20, 0),
        angle = 45,
        debug = debug
      ),
      axis.title.y = element_markdown(
        vjust = 0.5,
        margin = margin(0, 10, 0, 20),
        angle = 180,
        debug = debug
      )
    )
  
  p4 <- p +
    theme(
      axis.text.x = element_text(
        hjust = 1,
        margin = margin(10, 0, 20, 0),
        angle = 45,
        debug = debug
      ),
      axis.title.y = element_text(
        vjust = 0.5,
        margin = margin(0, 10, 0, 20),
        angle = 180,
        debug = debug
      )
    )
  
  expect_doppelganger(
    "Margins match w/ ggtext and ggplot2",
    cowplot::plot_grid(p1, p2, p3, p4)
  )
})
