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


<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;rect id=&quot;rect&quot; x=&quot;2&quot; y=&quot;2&quot; height=&quot;50&quot; width=&quot;50&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/rect&gt;&#10;&lt;/svg&gt;" alt="rect" width="50" height="50" title="rect"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;circle id=&quot;circle&quot; cx=&quot;27&quot; cy=&quot;27&quot; r=&quot;25&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/circle&gt;&#10;&lt;/svg&gt;" alt="circle" width="50" height="50" title="circle"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;polygon id=&quot;triangle&quot; points=&quot;2,52 52,52 27,2&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/polygon&gt;&#10;&lt;/svg&gt;" alt="triangle" width="50" height="50" title="triangle"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;polygon id=&quot;plus&quot; points=&quot;22,2 22,22 2,22 2,32 22,32 22,52 32,52 32,32 52,32 52,22 32,22 32,2 22,2&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/polygon&gt;&#10;&lt;/svg&gt;" alt="plus" width="50" height="50" title="plus"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;polygon id=&quot;cross&quot; points=&quot;9.07106781186548,2 2,9.07106781186548 19.9289321881345,27 2,44.9289321881345 9.07106781186548,52 27,34.0710678118655 44.9289321881345,52 52,44.9289321881345 34.0710678118655,27 52,9.07106781186548 44.9289321881345,2 27,19.9289321881345 9.07106781186548,2&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/polygon&gt;&#10;&lt;/svg&gt;" alt="cross" width="50" height="50" title="cross"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;polygon id=&quot;diamond&quot; points=&quot;27,2 2,27 27,52 52,27 27,2&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/polygon&gt;&#10;&lt;/svg&gt;" alt="diamond" width="50" height="50" title="diamond"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;polygon id=&quot;star&quot; points=&quot;22,2 22,14.92893 9.07107,2 2,9.07107 14.92893,22 2,22 2,32 14.92893,32 2,44.92893 9.07107,52 22,39.07107 22,52 32,52 32,39.07107 44.92893,52 52,44.92893 39.07107,32 52,32 52,22 39.07107,22 52,9.07107 44.92893,2 32,14.92893 32,2 22,2&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/polygon&gt;&#10;&lt;/svg&gt;" alt="star" width="50" height="50" title="star"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;rect id=&quot;stadium&quot; x=&quot;2&quot; y=&quot;2&quot; height=&quot;50&quot; width=&quot;50&quot; rx=&quot;25%&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/rect&gt;&#10;&lt;/svg&gt;" alt="stadium" width="50" height="50" title="stadium"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;line id=&quot;line&quot; x1=&quot;2&quot; x2=&quot;52&quot; y1=&quot;27&quot; y2=&quot;27&quot; stroke=&quot;black&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/line&gt;&#10;&lt;/svg&gt;" alt="line" width="50" height="50" title="line"/>
<img src="data:image/svg+xml,&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; version=&quot;1.1&quot; width=&quot;54&quot; height=&quot;54&quot;&gt;&#10;  &lt;polygon id=&quot;polygon&quot; points=&quot;27,2 3.22358709262116,19.2745751406263 12.3053686926882,47.2254248593737 41.6946313073118,47.2254248593737 50.7764129073788,19.2745751406263 27,2&quot; stroke=&quot;black&quot; fill=&quot;transparent&quot; stroke-opacity=&quot;1&quot; fill-opacity=&quot;1&quot; stroke-width=&quot;2&quot;&gt;&lt;/polygon&gt;&#10;&lt;/svg&gt;" alt="polygon" width="50" height="50" title="polygon"/>
