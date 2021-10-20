# leaflegend <a href='https://leaflegend.roh.engineering'><img src='man/figures/logo.png' align="right" height="106" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/leaflegend)](https://CRAN.R-project.org/package=leaflegend)
[![R-CMD-check](https://github.com/tomroh/leaflegend/workflows/R-CMD-check/badge.svg)](https://github.com/tomroh/leaflegend/actions)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://cranlogs.r-pkg.org/badges/grand-total/leaflegend?color=green)](https://cran.r-project.org/package=leaflegend)
<!-- badges: end -->

This package provides extensions to the leaflet package to 
customize leaflet legends without adding an outside css file to the output 
to style legends. The legend extensions allow the user to add images to 
legends, style the labels of the  legend items, change orientation of the 
legend items, use different symbologies, and style axis ticks. Syntax and
style is consistent with the leaflet package.

## Installation

You can install the released version of leaflegend from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("leaflegend")
```

Install the development version with:

```r
devtools::install_github("tomroh/leaflegend")
```
## Tutorials

* [Introduction to leaflegend](https://roh.engineering/posts/2021/02/introduction-to-leaflegend/)

* [Map Symbols and Size Legends](https://roh.engineering/posts/2021/05/map-symbols-and-size-legends-for-leaflet/)

## Example

Use `addLegend*()` to create easily customizable legends for leaflet.

``` r
library(leaflegend)
library(leaflet)
data(quakes)
quakes[['group']] <- sample(c('A', 'B', 'C'), nrow(quakes), replace = TRUE)
factorPal <- colorFactor('Dark2', quakes$group)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = quakes,
    lat = ~ lat,
    lng = ~ long,
    color = ~ factorPal(group),
    opacity = 1,
    fillOpacity = 1
  ) %>%
  addLegendFactor(
    pal = factorPal,
    title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px; color: red;'),
    labelStyle = 'font-size: 18px; font-weight: bold;',
    orientation = 'horizontal',
    values = quakes$group,
    position = 'topright',
    shape = 'triangle',
    width = 30,
    height = 30
  )
```

## Map Symbols

<img src="man/figures/rect.svg" alt="rect" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/circle.svg" alt="circle" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/triangle.svg" alt="triangle" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/plus.svg" alt="plus" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/cross.svg" alt="cross" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/diamond.svg" alt="diamond" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/star.svg" alt="star" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/stadium.svg" alt="stadium" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/line.svg" alt="line" width = 70 height = 70 style="margin: 5px;"></img>
<img src="man/figures/polygon.svg" alt="polygon" width = 70 height = 70 style="margin: 5px;"></img>
