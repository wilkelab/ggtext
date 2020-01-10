
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtext

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/clauswilke/ggtext.svg?branch=master)](https://travis-ci.org/clauswilke/ggtext)
[![Coverage
Status](https://img.shields.io/codecov/c/github/clauswilke/ggtext/master.svg)](https://codecov.io/github/clauswilke/ggtext?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggtext)](https://cran.r-project.org/package=ggtext)
<!-- badges: end -->

Improved text rendering support for ggplot2, written by Claus O. Wilke

This package provides ggplot2 support for the
[gridtext](https://github.com/clauswilke/gridtext) package, which
implements rich-text (basic HTML and Markdown) rendering for the grid
graphics engine. It is a work in progress. Many obvious features are
missing, and others have bugs or don’t work in corner cases.

## Installation

``` r
devtools::install_github("clauswilke/ggtext")
```

## Examples

Rich text formatting support is provided both in theme elements and via
geoms. All text rendering is performed via either `richtext_grob()` or
`textbox_grob()` from the [gridtext](https://wilkelab.org/gridtext)
package, and the featureset supported is equivalent to the features
provided by these grobs.

### Markdown in theme elements

The ggtext package defines two new theme elements, `element_markdown()`
and `element_textbox()`. Both behave similarly to `element_text()` but
render the provided text as markdown/html. `element_markdown()` is meant
as a direct replacement for `element_text()`, and it renders text
without word wrapping. To start a new line, use the `<br>` tag or add
two spaces before the end of a line.

``` r
library(ggplot2)
library(ggtext)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_manual(
    name = NULL,
    values = c(setosa = "#0072B2", virginica = "#009E73", versicolor = "#D55E00"),
    labels = c(
      setosa = "<i style='color:#0072B2'>I. setosa</i>",
      virginica = "<i style='color:#009E73'>I. virginica</i>",
      versicolor = "<i style='color:#D55E00'>I. versicolor</i>")
  ) +
  labs(
    title = "**Fisher's *Iris* dataset**  
    <span style='font-size:11pt'>Sepal width vs. sepal length for three *Iris*
    species</span>",
    x = "Sepal length (cm)", y = "Sepal width (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_markdown(size = 11)
  )
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

Very basic support for the `<img>` tag exists, and it can be used, for
example, to employ images as axis labels.

``` r
labels <- c(
  setosa = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/86/Iris_setosa.JPG/180px-Iris_setosa.JPG'
    width='100' /><br>*I. setosa*",
  virginica = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Iris_virginica_-_NRCS.jpg/320px-Iris_virginica_-_NRCS.jpg'
    width='100' /><br>*I. virginica*",
  versicolor = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/20140427Iris_versicolor1.jpg/320px-20140427Iris_versicolor1.jpg'
    width='100' /><br>*I. versicolor*"
)

ggplot(iris, aes(Species, Sepal.Width)) +
  geom_boxplot() +
  scale_x_discrete(
    name = NULL,
    labels = labels
  ) +
  theme(
    axis.text.x = element_markdown(color = "black", size = 11)
  )
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

`element_textbox()` offers support for rendering larger amounts of text
that require word wrapping. Unlike `element_markdown()`, it cannot be
used for axis tick labels, and it cannot draw text at arbitary angles,
only at fixed orientations corresponding to 0, 90, 180, and 270 degrees.
In practice, you will usually want to use `element_textbox_simple()`
instead of `element_textbox()`, as it sets useful defaults for many
parameters not usually defined in ggplot2 themes.

``` r
ggplot(mtcars, aes(disp, mpg)) + 
  geom_point() +
  labs(
    title = "<b>Fuel economy vs. engine displacement</b><br>
    <span style = 'font-size:10pt'>Lorem ipsum *dolor sit amet,*
    consectetur adipiscing elit, **sed do eiusmod tempor incididunt** ut
    labore et dolore magna aliqua. <span style = 'color:red;'>Ut enim
    ad minim veniam,</span> quis nostrud exercitation ullamco laboris nisi
    ut aliquip ex ea commodo consequat.</span>",
    x = "displacement (in<sup>3</sup>)",
    y = "Miles per gallon (mpg)<br><span style = 'font-size:8pt'>A measure of
    the car's fuel efficiency.</span>"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "cornsilk"
    ),
    axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt"),
      fill = "azure1"
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0),
      fill = "lightsteelblue1"
    )
  )
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

Another example, replacing facet strips with text boxes.

``` r
library(cowplot)

ggplot(mpg, aes(cty, hwy)) + 
  geom_point() +
  facet_wrap(~class) +
  theme_half_open(12) +
  background_grid() +
  theme(
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

### Geoms

The geom `geom_richtext()` provides markdown/html labels. Unlike
`geom_label()`, the labels can be rotated.

``` r
df <- data.frame(
  label = c(
    "Some text **in bold.**",
    "Linebreaks<br>Linebreaks<br>Linebreaks",
    "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
    "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
  ),
  x = c(.2, .1, .5, .9),
  y = c(.8, .4, .1, .5),
  hjust = c(0.5, 0, 0, 1),
  vjust = c(0.5, 1, 0, 0.5),
  angle = c(0, 0, 45, -45),
  color = c("black", "blue", "black", "red"),
  fill = c("cornsilk", "white", "lightblue1", "white")
)


ggplot(df) +
  aes(
    x, y, label = label, angle = angle, color = color, fill = fill,
    hjust = hjust, vjust = vjust
  ) +
  geom_richtext() +
  geom_point(color = "black", size = 2) +
  scale_color_identity() +
  scale_fill_identity() +
  xlim(0, 1) + ylim(0, 1)
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

Labels without frame or background are also possible.

``` r
ggplot(df) +
  aes(
    x, y, label = label, angle = angle, color = color,
    hjust = hjust, vjust = vjust
  ) +
  geom_richtext(
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  geom_point(color = "black", size = 2) +
  scale_color_identity() +
  xlim(0, 1) + ylim(0, 1)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

The geom `geom_textbox()` can draw boxes with word-wrapped text. It does
not support arbitrary rotation angles, only fixed orientations, just
like `element_textbox()`.

``` r
df <- data.frame(
  label = rep("Lorem ipsum dolor **sit amet,** consectetur adipiscing elit,
    sed do *eiusmod tempor incididunt* ut labore et dolore magna
    aliqua.", 2),
  x = c(0, .6),
  y = c(1, .6),
  hjust = c(0, 0),
  vjust = c(1, 0),
  orientation = c("upright", "right-rotated"),
  color = c("black", "blue"),
  fill = c("cornsilk", "white")
)

ggplot(df) +
  aes(
    x, y, label = label, color = color, fill = fill,
    box.hjust = hjust, box.vjust = vjust,
    orientation = orientation
  ) +
  geom_textbox(width = unit(0.4, "npc")) +
  geom_point(color = "black", size = 2) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) +
  xlim(0, 1) + ylim(0, 1)
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

## Acknowledgments

This project is receiving [financial
support](https://www.r-consortium.org/projects/awarded-projects) from
the [R
consortium.](https://www.r-consortium.org)

<img src="https://www.r-consortium.org/wp-content/uploads/sites/13/2016/09/RConsortium_Horizontal_Pantone.png" width="250">
