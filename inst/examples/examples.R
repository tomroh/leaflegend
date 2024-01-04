
# Toolkit -----------------------------------------------------------------

library(leaflet)
library(leaflegend)
data(quakes)

# Image Legend ------------------------------------------------------------

quakes1 <- quakes[1:10,]

leafIcons <- icons(
  iconUrl = ifelse(quakes1$mag < 4.6,
                   "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   "http://leafletjs.com/examples/custom-icons/leaf-red.png"
  ),
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes1) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons) %>%
  addLegendImage(images = c("http://leafletjs.com/examples/custom-icons/leaf-green.png",
                            "http://leafletjs.com/examples/custom-icons/leaf-red.png"),
                 labels = c('Green', 'Red'),width = 38, height = 95,
                 orientation = 'vertical',
                 title = htmltools::tags$div('Leaf',
                 style = 'font-size: 24px; text-align: center;'),
                 position = 'topright')

symbols <- Map(f = makeSymbol,
               shape = c('rect', 'circle', 'triangle', 'plus', 'cross', 'star')
               ,fillColor = c('blue', 'red', 'green', 'yellow', 'orange', 'purple')
               ,color = 'black'
               ,opacity = 1
               ,fillOpacity = .5
               ,height = 24
               ,width = 24
               ,'stroke-width' = 2)

leaflet(data = quakes1) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons) %>%
  addLegendImage(images = c(symbols, 'http://leafletjs.com/examples/custom-icons/leaf-green.png'),
                 labels = c('rect', 'circle', 'triangle', 'plus', 'cross', 'star', 'png'),width = 50, height = 50,
                 orientation = 'vertical',
                 title = htmltools::tags$div('Leaf',
                                             style = 'font-size: 24px; text-align: center;'),
                 position = 'topright')

# Numeric Legend ----------------------------------------------------------

library(leaflet)
data("quakes")
numPal <- colorNumeric('viridis', quakes$depth * 1000)
leaflet() %>%
  addTiles() %>%
  addLegendNumeric(
    pal = numPal,
    values = quakes$depth * 1000,
    position = 'topright',
    title = htmltools::tags$div('Horizontal',
                                style = 'font-size: 14px; text-align: center; margin-bottom: 5px;'),
    orientation = 'horizontal',
    shape = 'rect',
    decreasing = FALSE,
    height = 20,
    width = 100
  ) %>%
  addLegendNumeric(
    pal = numPal,
    values = quakes$depth*1000,
    position = 'topright',
    # title = htmltools::tags$div('addLegendNumeric (Decreasing)',
    #                             style = 'font-size: 24px; text-align: center; margin-bottom: 5px;'),
    orientation = 'vertical',
    shape = 'stadium',
    #decreasing = TRUE,
    height = 129,
    width = 18,
    #opacity = 1,
    fillOpacity = .5
  ) %>%
  addLegend(pal = numPal, values = quakes$depth * 1000)
x <- rnorm(nrow(quakes))
numPal <- colorNumeric('viridis', x)
leaflet() %>%
  addTiles() %>%
  addLegendNumeric(
    pal = numPal,
    values = x,
    position = 'topright',
    title = htmltools::tags$div('Horizontal',
                                style = 'font-size: 14px; text-align: center; margin-bottom: 5px;'),
    orientation = 'horizontal',
    shape = 'rect',
    decreasing = FALSE,
    height = 20,
    width = 100
  ) %>% addLegendNumeric(
    pal = numPal,
    values = x,
    position = 'topright',
    title = htmltools::tags$div('Vertical',
                                style = 'font-size: 14px; text-align: center; margin-bottom: 5px;'),
    orientation = 'vertical',
    shape = 'rect',
    decreasing = FALSE,
    height = 100,
    width = 20
  )
library(leaflet)
library(leaflegend)
data(quakes)
quakes1000 <- quakes[1:1000, ]
quakes1000[['x']] <- 1:1000
numPal <- colorNumeric('viridis', log10(c(.01, 1000)))
leaflet(quakes1000) %>%
  addTiles() %>%
  addCircleMarkers(color = ~numPal(log10(x))) %>%
  addLegendNumeric(
    pal = numPal,
    values = ~log10(x),
    position = 'topright',
    orientation = 'vertical',
    shape = 'stadium',
    height = 129,
    width = 18,
    bins = 5,
    fillOpacity = .5,
    numberFormat = function(x) {
      prettyNum(x^10, format = "f", big.mark = ",", digits =
          3, scientific = FALSE)
    }
  )
library(leaflet)
library(leaflegend)
data(quakes)
quakes1000 <- quakes[1:1000, ]
quakes1000[['x']] <- 1:1000
numPal <- colorNumeric('viridis', log10(quakes1000$depth))
leaflet(quakes1000) %>%
  addTiles() %>%
  addCircleMarkers(color = ~numPal(log10(depth))) %>%
  addLegendNumeric(
    pal = numPal,
    values = ~log10(depth),
    position = 'topright',
    orientation = 'vertical',
    shape = 'stadium',
    height = 129,
    width = 18,
    bins = 5,
    fillOpacity = .5,
    numberFormat = function(x) {
      prettyNum(x^10, format = "f", big.mark = ",", digits =
          3, scientific = FALSE)
    }
  )

library(leaflet)
library(leaflegend)
data(quakes)
quakes1000 <- quakes[1:1000, ]
quakes1000[['x']] <- 1:1000
quakes1000[['depth']][2] <- NA
numPal <- colorNumeric('viridis', (quakes1000$depth))
leaflet(quakes1000) %>%
  addTiles() %>%
  addCircleMarkers(color = ~numPal((depth))) %>%
  addLegendNumeric(
    pal = numPal,
    values = ~(depth),
    position = 'topright',
    orientation = 'vertical',
    shape = 'rect',
    decreasing = TRUE,
    height = 129,
    width = 18,
    bins = 5,
    title = 'depth',
    tickLength = 10,
    tickWidth = 5,
    fillOpacity = .5,
    numberFormat = function(x) {
      prettyNum(x, format = "f", big.mark = ",", digits =
          3, scientific = FALSE)
    }
  ) |>
  addLegendNumeric(
    pal = numPal,
    values = ~(depth),
    position = 'topright',
    orientation = 'vertical',
    shape = 'stadium',
    decreasing = FALSE,
    height = 129,
    width = 18,
    bins = 5,
    title = 'depth',
    tickLength = 10,
    labelStyle = 'font-size: 24px;',
    tickWidth = 5,
    fillOpacity = .5,
    numberFormat = function(x) {
      prettyNum(x, format = "f", big.mark = ",", digits =
          3, scientific = FALSE)
    }
  ) |>
  addLegendNumeric(
    pal = numPal,
    values = ~(depth),
    position = 'topright',
    orientation = 'horizontal',
    shape = 'stadium',
    height = 18,
    width = 129,
    bins = 5,
    title = 'depth',
    tickLength = 10,
    tickWidth = 5,
    fillOpacity = .5,
    numberFormat = function(x) {
      prettyNum(x, format = "f", big.mark = ",", digits =
          3, scientific = FALSE)
    }
  ) |>
  addLegendNumeric(
    pal = numPal,
    values = ~(depth),
    position = 'topright',
    orientation = 'horizontal',
    shape = 'rect',
    height = 18,
    width = 129,
    decreasing = TRUE,
    labels = c('high', 'low'),
    bins = 5,
    title = 'depth',
    tickLength = 10,
    tickWidth = 5,
    fillOpacity = .5,
    numberFormat = function(x) {
      prettyNum(x, format = "f", big.mark = ",", digits =
          3, scientific = FALSE)
    }
  )


numPal <- colorNumeric('viridis', quakes$depth)
leaflet() %>%
  addTiles() %>%
  addLegendNumeric(
    pal = numPal,
    values = quakes$depth,
    position = 'topright',
    title = htmltools::HTML('addLegendNumeric<br>(Horizontal)'),
    orientation = 'horizontal',
    shape = 'rect',
    decreasing = FALSE,
    height = 20,
    width = 100
  ) %>%
  addLegendNumeric(
    pal = numPal,
    values = quakes$depth,
    position = 'topright',
    title = htmltools::tags$div('addLegendNumeric (Decreasing)',
      style = 'font-size: 24px; text-align: center; margin-bottom: 5px;'),
    orientation = 'vertical',
    shape = 'stadium',
    decreasing = TRUE,
    height = 100,
    width = 20
  ) %>%
  addLegend(pal = numPal, values = quakes$depth, title = 'addLegend')

# Quantile Legend ---------------------------------------------------------

quakes$mag[1] <- NA
quantPal <- colorQuantile('viridis', quakes$mag, n = 5)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = quakes,
                   lat = ~lat,
                   lng = ~long,
                   color = ~quantPal(mag),
                   opacity = 1,
                   fillOpacity = 1
                   ) %>%
  addLegendQuantile(pal = quantPal,
                    values = quakes$mag,
                    position = 'topright',
                    title = 'addLegendQuantile',
                    numberFormat = function(x) {prettyNum(x, big.mark = ',', scientific = FALSE, digits = 2)},
                    shape = 'circle') %>%
  addLegendQuantile(pal = quantPal,
                    values = quakes$mag,
                    position = 'topright',
                    title = htmltools::tags$div('addLegendQuantile',
                                                htmltools::tags$br(),
                                                '(Omit Numbers)'),
                    numberFormat = NULL,
                    shape = 'rect') %>%
  addLegend(pal = quantPal, values = quakes$mag, title = 'addLegend')

# Factor Legend -----------------------------------------------------------

quakes[['group']] <- factor(sample(c('A', 'B', 'C'), nrow(quakes), replace = TRUE),
                            levels = c('B', 'A', 'C'))

factorPal <- colorFactor('Dark2', quakes$group)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = quakes,
    lat = ~ lat,
    lng = ~ long,
    color = ~ factorPal(group),
    opacity = 1,
    fillOpacity = .5,
    weight = 1,
    popup = ~group
  ) %>%
  addLegendFactor(
    pal = factorPal,
    title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px; color: red;'),
    values = quakes$group,
    position = 'topright',
    shape = 'triangle',
    width = 30,
    height = 30,
    opacity = .5
  ) %>%
  addLegend(pal = factorPal,
            values = quakes$group,
            title = 'addLegend')

# Bin Legend --------------------------------------------------------------

binPal <- colorBin('Set1', quakes$mag)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = quakes,
    lat = ~ lat,
    lng = ~ long,
    color = ~ binPal(mag),
    opacity = 1,
    fillOpacity = 1
  ) %>%
  addLegendBin(
    pal = binPal,
    values = quakes$mag,
    position = 'topright',
    title = 'addLegendBin',
    labelStyle = 'font-size: 18px; font-weight: bold;',
    orientation = 'horizontal'
  ) %>%
  addLegend(pal = binPal,
            values = quakes$mag,
            title = 'addLegend')

symbols <- lapply(binPal(quakes$mag)
                  ,makeSymbol
                  ,shape = 'triangle'
                  #,color = 'black'
                  #,fillColor = binPal(qua
                  ,opacity = 0
                  ,fillOpacity = .5
                  ,height = 24
                  ,width = 24
                  #,stroke = 'black'
                  ,'stroke-width' = 2)
leaflet() %>%
  addTiles() %>%
  addMarkers(
    data = quakes,
    lat = ~ lat,
    lng = ~ long,
    icon = ~icons(
      iconUrl = symbols,
      iconWidth = 50,
      iconHeight = 50
    )
  ) %>%
  addLegendBin(
    pal = binPal,
    values = quakes$mag,
    position = 'topright',
    title = 'addLegendBin',
    shape = 'triangle',
    opacity = 1,
    fillOpacity = .2,
    labelStyle = 'font-size: 18px; font-weight: bold;',
    orientation = 'vertical'
  ) %>%
  addLegend(pal = binPal,
            values = quakes$mag,
            title = 'addLegend')

# README ------------------------------------------------------------------

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
    title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px; color: red;margin-bottom:5px;'),
    labelStyle = 'font-size: 18px; font-weight: bold;',
    orientation = 'horizontal',
    values = quakes$group,
    position = 'topright',
    shape = 'triangle',
    width = 30,
    height = 30
  )


# Borders -----------------------------------------------------------------

library(leaflet)
data(quakes)
quakes2 <- quakes[1:2, ]
s1 <- makeSymbol('circle', width = 24, height = 24, color = 'black', fillColor = 'blue', 'stroke-width' = 2)
leaflet() %>%
  addTiles() %>%
  addLegendImage(images = list(s1), labels = c('outline'))


# Legend Size -------------------------------------------------------------

library(leaflet)
data("quakes")
numPal <- colorNumeric('viridis', 10^(quakes$mag))
leaflet(quakes) %>%
  addTiles() %>%
  addSymbolsSize(values = ~10^(mag),
    lat = ~lat,
    lng = ~long,
    shape = 'plus',
    color = ~numPal(10^(mag)),
    fillColor = ~numPal(10^(mag)),
    opacity = .5,
    baseSize = 1) %>%
  addLegendSize(
    values = ~10^(mag),
    pal = numPal,
    title = 'Magnitude',
    baseSize = 1,
    shape = 'plus',
    orientation = 'horizontal',
    opacity = .5,
    fillOpacity = .3,
    position = 'bottomleft',
    breaks = 5) %>%
  addLegendSize(
    values = ~10^(mag),
    pal = numPal,
    title = 'Magnitude',
    baseSize = 1,
    shape = 'plus',
    orientation = 'horizontal',
    opacity = .5,
    fillOpacity = .3,
    position = 'bottomleft',
    breaks = stats::setNames(seq(500000, 2500000, 500000),
      c('Very Small', 'Small', 'Medium', 'Large', 'Very Large'))
  ) |>
  addLegendSize(
    values = ~10^(mag),
    pal = numPal,
    title = 'Magnitude',
    baseSize = 1,
    shape = 'plus',
    orientation = 'vertical',
    opacity = .5,
    fillOpacity = .3,
    position = 'bottomleft',
    breaks = stats::setNames(seq(500000, 2500000, 500000),
      c('Very Small', 'Small', 'Medium', 'Large', 'Very Large'))
  )

# Stacked Size Legend -----------------------------------------------------

library(leaflet)
data("quakes")
numPal <- colorNumeric('viridis', 10^(quakes$mag))
baseSize <- 5
strokeWidth <- 1
leaflet(quakes) %>%
  addTiles() %>%
  addSymbolsSize(values = ~10^(mag),
    lat = ~lat,
    lng = ~long,
    shape = 'circle',
    color = 'black',
    fillColor = 'red',
    opacity = 1,
    baseSize = baseSize,
    strokeWidth = strokeWidth) |>
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'circle',
    color = 'black',
    fillColor = 'red',
    labelStyle = 'font-size: 24px;',
    position = 'bottomleft',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = strokeWidth) |>
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'circle',
    color = 'black',
    fillColor = 'red',
    labelStyle = 'font-size: 8px;',
    position = 'bottomleft',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = strokeWidth) |>
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'circle',
    color = 'black',
    fillColor = 'red',
    labelStyle = 'font-size: 16px;',
    position = 'bottomleft',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = strokeWidth)

leaflet(quakes) %>%
  addTiles() %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'circle',
    color = 'black',
    fillColor = 'red',
    position = 'bottomleft',
    labelStyle = 'font-size: 20px;',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 1) %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'triangle',
    color = 'black',
    fillColor = 'red',
    position = 'bottomleft',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 2) %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'diamond',
    color = 'black',
    fillColor = 'red',
    position = 'bottomleft',
    labelStyle = 'color: black;',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 3) %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'stadium',
    color = 'black',
    fillColor = 'red',
    position = 'bottomright',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 4) %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'polygon',
    color = 'black',
    fillColor = 'red',
    position = 'bottomright',
    labelStyle = 'font-size: 24px;',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 5) %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'rect',
    color = 'black',
    fillColor = 'red',
    position = 'bottomleft',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 6) %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'star',
    color = 'black',
    fillColor = 'red',
    position = 'bottomright',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 2) %>%
  # addLegendSize(
  #   values = ~10^(mag),
  #   title = 'Magnitude',
  #   baseSize = baseSize,
  #   shape = 'plus',
  #   color = 'black',
  #   fillColor = 'red',
  #   position = 'bottomright',
  #   stacked = TRUE,
  #   breaks = 5,
  #   strokeWidth = 2) %>%
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'cross',
    color = 'black',
    fillColor = 'red',
    position = 'bottomright',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 2)


library(leaflet)
library(leaflegend)
data("quakes")
baseSize <- 5
numPal <- colorNumeric('viridis', 10^(quakes$mag))
leaflet(quakes) |>
  addTiles() |>
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'circle',
    color = 'black',
    fillColor = 'red',
    position = 'topright',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 2) |>
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'triangle',
    color = 'black',
    fillColor = 'red',
    position = 'topright',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 2) |>
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'diamond',
    color = 'black',
    fillColor = 'red',
    position = 'topright',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 2) |>
  addLegendSize(
    values = ~10^(mag),
    title = 'Magnitude',
    baseSize = baseSize,
    shape = 'stadium',
    color = 'black',
    fillColor = 'red',
    position = 'topright',
    stacked = TRUE,
    breaks = 5,
    strokeWidth = 2)

# Group Layers ------------------------------------------------------------
library(leaflet)
library(leaflegend)
numPal <- colorNumeric('viridis', quakes$depth)
quantPal <- colorQuantile('viridis', quakes$mag, n = 5)
binPal <- colorBin('Set1', quakes$mag)
leaflet() %>%
  addTiles() %>%
  addMarkers(data = quakes, group = 'Bin') %>%
  addLegendNumeric(
    pal = numPal,
    values = quakes$depth,
    position = 'topright',
    title = 'addLegendNumeric',
    group = 'Numeric Data'
  ) %>%
  addLegendQuantile(
    pal = quantPal,
    values = quakes$mag,
    position = 'topright',
    title = 'addLegendQuantile',
    group = 'Quantile'
  ) %>%
  addLegendBin(
    pal = binPal,
    values = quakes$mag,
    position = 'bottomleft',
    title = 'addLegendBin',
    group = 'Bin'
  ) %>%
  addLayersControl(
    baseGroups = c('Numeric Data', 'Quantile'),  overlayGroups = c('Bin'),
    position = 'bottomright'
  )

# Multipe Image Legend Sizes ----------------------------------------------
library(leaflet)
data(quakes)

height <- sizeNumeric(quakes$depth, baseSize = 40)
width <- height * 38 / 95
symbols <- icons(
  iconUrl = 'http://leafletjs.com/examples/custom-icons/leaf-green.png',
  iconWidth = width,
  iconHeight = height)
probs <- c(.2, .4, .6, .8)
leaflet(quakes) %>%
  addTiles() %>%
  addMarkers(icon = symbols,
             lat = ~lat, lng = ~long) %>%
  addLegendImage(images = rep("http://leafletjs.com/examples/custom-icons/leaf-green.png", 4),
                 labels = round(quantile(height, probs = probs), 0),
                 width = quantile(height, probs = probs) * 38 / 95,
                 height = quantile(height, probs = probs),
                 title = htmltools::tags$div('Leaf',
                                             style = 'font-size: 24px; text-align: center;'),
                 position = 'topright', orientation = 'vertical')

# Vary Text ---------------------------------------------------------------
library(leaflet)
library(leaflegend)
data(quakes)
fontSize <- c('small' = 10, 'medium' = 20 , 'large' = 30)
leaflet(quakes) |> addLegendSize(values = ~depth, color = 'black', shape  = 'plus',
    breaks = 3, labelStyle = sprintf('font-size: %dpx;vertical-align: middle;',
      fontSize['medium'])) |>
  addLegendSize(values = ~depth, color = 'black', shape  = 'plus',
    breaks = 3, labelStyle = sprintf('font-size: %dpx;vertical-align: middle;',
      fontSize['large']),
    orientation = 'horizontal')


# Multi-Column ------------------------------------------------------------

symbols <- Map(f = makeSymbol,
  shape = c('rect', 'circle', 'triangle', 'plus', 'cross', 'star')
  ,fillColor = c('blue', 'red', 'green', 'yellow', 'orange', 'purple')
  ,color = 'black'
    ,opacity = 1
  ,fillOpacity = .5
  ,height = 24
  ,width = 24
  ,'stroke-width' = 2)
m <- leaflet(x) |>
  addLegendImage(title = 'Legend', images = symbols[1:3], labels = 1:3, position = 'topright') |>
  addLegendImage(title = htmltools::HTML('<br>'), images = symbols[4:6], labels = 4:6, position = 'topright')
htmltools::browsable(
  htmltools::tagList(
    htmltools::tags$style('
      div.leaflet-top.leaflet-right{
      display: flex;
      }
      .info.legend.leaflet-control:first-child {
      margin-right: 0px;
      border-radius: 0px;
      background-color: white;
      box-shadow: none;
      }
      .info.legend.leaflet-control {
      border-radius: 0px;
      background-color: white;
      box-shadow: none;
      }'),
    m
  )
)



# Pch Symbols -------------------------------------------------------------

pchNames <- stats::setNames(seq(0L, 25L, 1L),
  c('open-rect', 'open-circle', 'open-triangle', 'simple-plus',
    'simple-cross', 'open-diamond', 'open-down-triangle', 'cross-rect',
    'simple-star', 'plus-diamond', 'plus-circle', 'hexagram', 'plus-rect',
    'cross-circle', 'triangle-rect', 'solid-rect', 'solid-circle-md',
    'solid-triangle', 'solid-diamond', 'solid-circle-bg', 'solid-circle-sm', 'circle',
    'rect', 'diamond', 'triangle', 'down-triangle'
  ))
defaultSize <- 20
i <- 1:26
pchSvg <- lapply(names(pchNames)[i], makePch, width = defaultSize,
  color = 'black', `stroke-width` = 2, fillOpacity = .5)
pchSvgI <- lapply(i-1, makePch, width = defaultSize,
  color = 'black', `stroke-width` = 2, fillOpacity = .5)
leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
  addLegendImage(images = pchSvg, labels =names(pchNames),
    width = defaultSize, height = defaultSize, position = 'topright') |>
  addLegendImage(images = pchSvgI, labels = i-1,
    width = defaultSize, height = defaultSize, position = 'topleft')
