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

<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <rect id="rect" x="2" y="2" height="50" width="50" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></rect>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <circle id="circle" cx="27" cy="27" r="25" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></circle>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <polygon id="triangle" points="2,52 52,52 27,2" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></polygon>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <polygon id="plus" points="22,2 22,22 2,22 2,32 22,32 22,52 32,52 32,32 52,32 52,22 32,22 32,2 22,2" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></polygon>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <polygon id="cross" points="9.07106781186548,2 2,9.07106781186548 19.9289321881345,27 2,44.9289321881345 9.07106781186548,52 27,34.0710678118655 44.9289321881345,52 52,44.9289321881345 34.0710678118655,27 52,9.07106781186548 44.9289321881345,2 27,19.9289321881345 9.07106781186548,2" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></polygon>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <polygon id="diamond" points="27,2 2,27 27,52 52,27 27,2" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></polygon>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <polygon id="star" points="22,2 22,14.92893 9.07107,2 2,9.07107 14.92893,22 2,22 2,32 14.92893,32 2,44.92893 9.07107,52 22,39.07107 22,52 32,52 32,39.07107 44.92893,52 52,44.92893 39.07107,32 52,32 52,22 39.07107,22 52,9.07107 44.92893,2 32,14.92893 32,2 22,2" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></polygon>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <rect id="stadium" x="2" y="2" height="50" width="50" rx="25%" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></rect>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <line id="line" x1="2" x2="52" y1="27" y2="27" stroke="black" stroke-opacity="1" fill-opacity="1" stroke-width="2"></line>
</svg>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="54" height="54">
  <polygon id="polygon" points="27,2 3.22358709262116,19.2745751406263 12.3053686926882,47.2254248593737 41.6946313073118,47.2254248593737 50.7764129073788,19.2745751406263 27,2" stroke="black" fill="transparent" stroke-opacity="1" fill-opacity="1" stroke-width="2"></polygon>
</svg>
