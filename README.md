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

<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%222%22%20y%3D%222%22%20height%3D%2250%22%20width%3D%2250%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Frect%3E%0A%3C%2Fsvg%3E" alt="rect" width="50" height="50" title="rect"/>
  <p style="text-align: center;">rect</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Ccircle%20id%3D%22circle%22%20cx%3D%2227%22%20cy%3D%2227%22%20r%3D%2225%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fcircle%3E%0A%3C%2Fsvg%3E" alt="circle" width="50" height="50" title="circle"/>
  <p style="text-align: center;">circle</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Cpolygon%20id%3D%22triangle%22%20points%3D%222%2C52%2052%2C52%2027%2C2%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fpolygon%3E%0A%3C%2Fsvg%3E" alt="triangle" width="50" height="50" title="triangle"/>
  <p style="text-align: center;">triangle</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Cpolygon%20id%3D%22plus%22%20points%3D%2222%2C2%2022%2C22%202%2C22%202%2C32%2022%2C32%2022%2C52%2032%2C52%2032%2C32%2052%2C32%2052%2C22%2032%2C22%2032%2C2%2022%2C2%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fpolygon%3E%0A%3C%2Fsvg%3E" alt="plus" width="50" height="50" title="plus"/>
  <p style="text-align: center;">plus</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Cpolygon%20id%3D%22cross%22%20points%3D%229.07106781186548%2C2%202%2C9.07106781186548%2019.9289321881345%2C27%202%2C44.9289321881345%209.07106781186548%2C52%2027%2C34.0710678118655%2044.9289321881345%2C52%2052%2C44.9289321881345%2034.0710678118655%2C27%2052%2C9.07106781186548%2044.9289321881345%2C2%2027%2C19.9289321881345%209.07106781186548%2C2%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fpolygon%3E%0A%3C%2Fsvg%3E" alt="cross" width="50" height="50" title="cross"/>
  <p style="text-align: center;">cross</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Cpolygon%20id%3D%22diamond%22%20points%3D%2227%2C2%202%2C27%2027%2C52%2052%2C27%2027%2C2%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fpolygon%3E%0A%3C%2Fsvg%3E" alt="diamond" width="50" height="50" title="diamond"/>
  <p style="text-align: center;">diamond</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Cpolygon%20id%3D%22star%22%20points%3D%2222%2C2%2022%2C14.92893%209.07107%2C2%202%2C9.07107%2014.92893%2C22%202%2C22%202%2C32%2014.92893%2C32%202%2C44.92893%209.07107%2C52%2022%2C39.07107%2022%2C52%2032%2C52%2032%2C39.07107%2044.92893%2C52%2052%2C44.92893%2039.07107%2C32%2052%2C32%2052%2C22%2039.07107%2C22%2052%2C9.07107%2044.92893%2C2%2032%2C14.92893%2032%2C2%2022%2C2%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fpolygon%3E%0A%3C%2Fsvg%3E" alt="star" width="50" height="50" title="star"/>
  <p style="text-align: center;">star</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Crect%20id%3D%22stadium%22%20x%3D%222%22%20y%3D%222%22%20height%3D%2250%22%20width%3D%2250%22%20rx%3D%2225%25%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Frect%3E%0A%3C%2Fsvg%3E" alt="stadium" width="50" height="50" title="stadium"/>
  <p style="text-align: center;">stadium</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Cline%20id%3D%22line%22%20x1%3D%222%22%20x2%3D%2252%22%20y1%3D%2227%22%20y2%3D%2227%22%20stroke%3D%22black%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fline%3E%0A%3C%2Fsvg%3E" alt="line" width="50" height="50" title="line"/>
  <p style="text-align: center;">line</p>
</div>
<div style="width: 50px; display: inline-block;">
  <img src="data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2254%22%20height%3D%2254%22%3E%0A%20%20%3Cpolygon%20id%3D%22polygon%22%20points%3D%2227%2C2%203.22358709262116%2C19.2745751406263%2012.3053686926882%2C47.2254248593737%2041.6946313073118%2C47.2254248593737%2050.7764129073788%2C19.2745751406263%2027%2C2%22%20stroke%3D%22black%22%20fill%3D%22transparent%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%221%22%20stroke-width%3D%222%22%3E%3C%2Fpolygon%3E%0A%3C%2Fsvg%3E" alt="polygon" width="50" height="50" title="polygon"/>
  <p style="text-align: center;">polygon</p>
</div>
