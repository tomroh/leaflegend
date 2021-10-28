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

* [Awesome Marker Legends](https://roh.engineering/posts/2021/10/awesome-marker-legends-in-leaflet/)

## Example

Use `addLegend*()` to create easily customizable legends for leaflet.

``` r

library(leaflet)
library(leaflegend)
set.seed(22)
data("gadmCHE")
gadmCHE@data$x <- sample(c('A', 'B', 'C'), nrow(gadmCHE@data), replace = TRUE)
factorPal <- colorFactor('Set1', gadmCHE@data$x)
n <- 10
awesomeMarkers <- data.frame(
  marker = sample(c('Font Awesome', 'Ionic', 'Glyphicon'), n, replace = TRUE),
  lng = runif(n, gadmCHE@bbox[1,1], gadmCHE@bbox[1,2]),
  lat = runif(n, gadmCHE@bbox[2,1], gadmCHE@bbox[2,2])
)
n2 <- 30
symbolMarkers <- data.frame(
  x = runif(n2, 0, 100),
  lng = runif(n2, gadmCHE@bbox[1,1], gadmCHE@bbox[1,2]),
  lat = runif(n2, gadmCHE@bbox[2,1], gadmCHE@bbox[2,2])
)
numericPal <- colorNumeric('inferno', symbolMarkers$marker)
iconSet <- awesomeIconList(
  `Font Awesome` = makeAwesomeIcon(icon = "font-awesome", library = "fa",
                                   iconColor = 'gold', markerColor = 'red',
                                   squareMarker = TRUE,
                                   iconRotate = 30,
  ),
  Ionic = makeAwesomeIcon(icon = "ionic", library = "ion",
                          iconColor = '#fffff', markerColor = 'blue',
                          squareMarker = FALSE),
  Glyphicon = makeAwesomeIcon(icon = "plus-sign", library = "glyphicon",
                              iconColor = 'rgb(192, 255, 0)', markerColor = 'darkpurple',
                              squareMarker = FALSE)
)
baseSize <- 20
symbols <- makeSizeIcons(value = symbolMarkers$x,
                         'plus',
                         fillColor = numericPal(symbolMarkers$x),
                         baseSize = baseSize,
                         opacity = 1,
                         color = 'black')
leaflet() |>
  addTiles() |>
  addPolygons(data = gadmCHE, color = ~factorPal(x),
              fillOpacity = .5) |>
  addAwesomeMarkers(data = awesomeMarkers, lat = ~lat, lng = ~lng,
                    icon = ~iconSet[marker]) |>
  addMarkers(data = symbolMarkers, lat = ~lat, lng = ~lng, icon = symbols) |>
  addLegendFactor(pal = factorPal,
                  shape = 'polygon',
                  values = gadmCHE@data$x,
                  title = 'addLegendFactor',
                  position = 'topright') |>
  addLegendAwesomeIcon(iconSet = iconSet,
                       title = 'addLegendAwesomeIcon',
                       position = 'bottomleft') |>
  addLegendSize(pal = numericPal,
                shape = 'plus',
                color = 'black',
                baseSize = 20,
                values = symbolMarkers$x,
                orientation = 'horizontal',
                title = 'addSizeLegend',
                position = 'bottomright') |>
  addLegendNumeric(pal = numericPal,
                   values = symbolMarkers$x,
                   title = 'addLegendNumeric',
                   orientation = 'horizontal',
                   width = 150,
                   height = 20,
                   position = 'bottomright')
```

<img src="man/figures/readme-example.png"></img>

## Map Symbols


<img src="man/figures/rect.svg" alt="rect" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/circle.svg" alt="circle" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/triangle.svg" alt="triangle" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/plus.svg" alt="plus" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/cross.svg" alt="cross" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/diamond.svg" alt="diamond" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/star.svg" alt="star" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/stadium.svg" alt="stadium" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/line.svg" alt="line" width = 50 height = 70 style="margin: 5px;"></img>
<img src="man/figures/polygon.svg" alt="polygon" width = 50 height = 70 style="margin: 5px;"></img>
