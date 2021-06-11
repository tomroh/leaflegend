library(leaflet)
data(quakes)

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

numPal <- colorNumeric('viridis', quakes$depth)

quantPal <- colorQuantile('viridis', quakes$mag, n = 5)

quakes[['group']] <- sample(c('A', 'B', 'C'), nrow(quakes), replace = TRUE)
factorPal <- colorFactor('Dark2', quakes$group)

binPal <- colorBin('Set1', quakes$mag)

quakes2 <- quakes[1:100,]
numPal2 <- colorNumeric('viridis', quakes$depth)
sizes <- sizeNumeric(quakes$depth, baseSize = 10)
symbols <- Map(
  makeSymbol,
  shape = 'triangle',
  color = numPal(quakes$depth),
  width = sizes,
  height = sizes
)

leaflet(data = quakes1) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons, group = 'addLegendImage') %>%
  addLegendImage(images = c("http://leafletjs.com/examples/custom-icons/leaf-green.png",
                            "http://leafletjs.com/examples/custom-icons/leaf-red.png"),
                 labels = c('Green', 'Red'),width = 38, height = 95,
                 title = htmltools::tags$div('Leaf',
                 style = 'font-size: 24px; text-align: center;'),
                 position = 'bottomleft',
                 group = 'addLegendImage') %>%
  addLegendNumeric(
    pal = numPal,
    values = quakes$depth,
    position = 'bottomleft',
    title = 'addLegendNumeric (Horizontal)',
    orientation = 'horizontal',
    shape = 'rect',
    decreasing = FALSE,
    height = 20,
    width = 100,
    group = 'addLegendNumeric'
  ) %>%
  addLegendNumeric(
    pal = numPal,
    values = quakes$depth,
    position = 'bottomleft',
    title = htmltools::tags$div('addLegendNumeric (Decreasing)',
                                style = 'font-size: 24px; text-align: center; margin-bottom: 5px;'),
    orientation = 'vertical',
    shape = 'stadium',
    decreasing = TRUE,
    height = 100,
    width = 20,
    group = 'addLegendNumeric'
  ) %>%
  addLegend(pal = numPal, values = quakes$depth, title = 'addLegend (numeric)', group = 'addLegend', position = 'bottomright') %>%
  addCircleMarkers(data = quakes,
                  lat = ~lat,
                  lng = ~long,
                  color = ~quantPal(mag),
                  opacity = 1,
                  fillOpacity = 1,
                  group = 'addLegendQuantile'
  ) %>%
  addLegendQuantile(pal = quantPal,
                   values = quakes$mag,
                   position = 'topright',
                   title = 'addLegendQuantile',
                   numberFormat = function(x) {prettyNum(x, big.mark = ',',
                   scientific = FALSE, digits = 2)},
                   shape = 'circle',
                   group = 'addLegendQuantile') %>%
  addLegendQuantile(pal = quantPal,
                   values = quakes$mag,
                   position = 'topright',
                   title = htmltools::tags$div('addLegendQuantile',
                                               htmltools::tags$br(),
                                               '(Omit Numbers)'),
                   numberFormat = NULL,
                   shape = 'circle',
                   group = 'addLegendQuantile') %>%
  addLegend(pal = quantPal, values = quakes$mag, title = 'addLegend (quantile)',
            group = 'addLegend', position = 'bottomright') %>%
  addCircleMarkers(
   data = quakes,
   lat = ~ lat,
   lng = ~ long,
   color = ~ factorPal(group),
   opacity = 1,
   fillOpacity = 1,
   group = 'addLegendFactor'
  ) %>%
  addLegendFactor(
   pal = factorPal,
   title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px; color: red;'),
   values = quakes$group,
   position = 'topright',
   shape = 'triangle',
   width = 50,
   height = 50,
   group = 'addLegendFactor'
  ) %>%
  addLegend(pal = factorPal,
           values = quakes$group,
           title = 'addLegend (factor)',
           group = 'addLegend',
           position = 'bottomright') %>%
  addCircleMarkers(
   data = quakes,
   lat = ~ lat,
   lng = ~ long,
   color = ~ binPal(mag),
   opacity = 1,
   fillOpacity = 1,
   group = 'addLegendBin'
  ) %>%
  addLegendBin(
   pal = binPal,
   values = quakes$mag,
   position = 'topright',
   title = 'addLegendBin',
   labelStyle = 'font-size: 18px; font-weight: bold;',
   orientation = 'horizontal',
   group = 'addLegendBin'
  ) %>%
  addLegend(pal = binPal,
           values = quakes$mag,
           title = 'addLegend (bin)',
           group = 'addLegend',
           position = 'bottomright') %>%
  addMarkers(data = quakes2,
             icon = icons(iconUrl = symbols),
             lat = ~lat, lng = ~long) %>%
  addLegendSize(
    values = quakes2$depth,
    pal = numPal2,
    title = 'Depth',
    labelStyle = 'margin: auto;',
    shape = c('triangle'),
    orientation = c('vertical', 'horizontal'),
    opacity = .7,
    breaks = 5,
    position = 'topright',
    group = 'addLegendSize') %>%
  addLayersControl(
    baseGroups = c('addLegendQuantile', 'addLegendFactor', 'addLegendBin', 'addLegendSize'),
    overlayGroups = c('addLegend', 'addLegendImage', 'addLegendNumeric'),
    position = 'topleft'
  )
