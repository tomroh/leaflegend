
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

# Quantile Legend ---------------------------------------------------------

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
                    shape = 'circle') %>%
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
    orientation = 'horizontal'
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
    title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px; color: red;'),
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
numPal <- colorNumeric('viridis', quakes$depth)
symbols <- makeSizeIcons(values = quakes$mag,
                         shape = 'triangle',
                         pal = numPal,
                         color = 'black',
                         colorValues = quakes$depth,
                         baseSize = 10,
                         opacity = .5)
symbols <- makeSizeIcons(values = quakes$mag,
                         shape = 'triangle',
                         pal = numPal,
                         #color = 'black',
                         colorValues = quakes$depth,
                         baseSize = 10,
                         opacity = .5,
                         fillOpacity = 1)
leaflet() %>%
  addTiles() %>%
  addMarkers(data = quakes,
             icon = symbols,
             lat = ~lat, lng = ~long) %>%
  addLegendSize(
    values = quakes$mag,
    color = 'black',
    title = 'Magnitude',
    labelStyle = 'margin: auto;',
    shape = c('triangle'),
    orientation = 'horizontal',
    opacity = .7,
    breaks = 5) %>%
  addLegendSize(
    values = quakes$mag,
    color = 'black',
    fillColor = 'transparent',
    title = 'Magnitude',
    labelStyle = 'margin: auto;',
    shape = c('triangle'),
    orientation = 'horizontal',
    opacity = .7,
    breaks = 5) %>%
  addLegendNumeric(values = quakes$depth,
                   pal = numPal,
                   orientation = 'horizontal',
                   width = 100,
                   height = 18,
                   title = 'Depth') %>%
  addLegendSize(
    values = quakes$depth,
    pal = numPal,
    title = 'Depth',
    labelStyle = 'margin: auto;',
    shape = c('cross'),
    orientation ='horizontal',
    opacity = .7,
    color = 'black',
    breaks = 5) %>%
  addLegendSize(
    values = quakes$depth,
    pal = numPal,
    title = 'Depth',
    labelStyle = 'margin: auto;',
    shape = c('cross'),
    orientation ='horizontal',
    opacity = .7,
    fillColor = 'black',
    breaks = 5) %>%
  addLegendSize(
    values = quakes$depth,
    pal = numPal,
    title = 'Depth',
    labelStyle = 'margin: auto;',
    shape = c('cross'),
    orientation ='horizontal',
    opacity = .7,
    breaks = 5)

# Group Layers ------------------------------------------------------------
numPal <- colorNumeric('viridis', quakes$depth)
quantPal <- colorQuantile('viridis', quakes$mag, n = 5)
binPal <- colorBin('Set1', quakes$mag)
leaflet() %>%
  addTiles() %>%
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
