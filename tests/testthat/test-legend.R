testthat::test_that('Symbols', {
  # test args
  makeSymbol('notashape', width = 1, color = 'black') %>%
    testthat::expect_error()
  makeSymbol('rect', width = '1', color = 'black') %>%
    testthat::expect_error()
  makeSymbol('rect', width = 1, height = '1', color = 'black') %>%
    testthat::expect_error()
  makeSymbol('rect', width = 1, opacity = '1', color = 'black') %>%
    testthat::expect_error()
  makeSymbol('rect', width = 1, opacity = 1, fillOpacity = '1',
             color = 'black') %>%
    testthat::expect_error()
  makeSymbolIcons('notashape', width = 1, color = 'black') %>%
    testthat::expect_error()
  makeSymbolIcons('rect', width = '1', color = 'black') %>%
    testthat::expect_error()
  makeSymbolIcons('rect', width = 1, height = '1', color = 'black') %>%
    testthat::expect_error()
  makeSymbolIcons('rect', width = 1, opacity = '1', color = 'black') %>%
    testthat::expect_error()
  makeSymbolIcons('rect', width = 1, opacity = 1, fillOpacity = '1',
                  color = 'black') %>%
    testthat::expect_error()
  makeSymbolIcons(NA, width = 1, height = 1, opacity = 1, color = 'black') %>%
    testthat::expect_error()
  makeLegendSymbol(shape = 'rect', width = 1, color = 'black') %>%
    testthat::expect_error()
  makeLegendSymbol(label = '', shape = 'rect', width = 1, color = 'black') %>%
    testthat::expect_error()
  # test shapes
  makeSymbol('rect', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
      testthat::expect_equal(
        'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <rect id=\"rect\" x=\"1\" y=\"1\" height=\"2\" width=\"1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></rect>\n</svg>'
      )
  makeSymbol('circle', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <circle id=\"circle\" cx=\"2\" cy=\"2\" r=\"1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></circle>\n</svg>'
    )
  makeSymbol('triangle', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"triangle\" points=\"1,3 2,3 1.5,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>'
    )
  makeSymbol('plus', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"plus\" points=\"1.4,1 1.4,1.8 1,1.8 1,2.2 1.4,2.2 1.4,3 1.6,3 1.6,2.2 2,2.2 2,1.8 1.6,1.8 1.6,1 1.4,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>'
      )
  makeSymbol('cross', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"cross\" points=\"1.14142135623731,1 1,1.28284271247462 1.35857864376269,2 1,2.71715728752538 1.14142135623731,3 1.5,2.28284271247462 1.85857864376269,3 2,2.71715728752538 1.64142135623731,2 2,1.28284271247462 1.85857864376269,1 1.5,1.71715728752538 1.14142135623731,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>'
      )
  makeSymbol('diamond', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"diamond\" points=\"1.5,1 1,2 1.5,3 2,2 1.5,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>'
      )
  makeSymbol('star', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"star\" points=\"1.4,1 1.4,1.5171572 1.1414214,1 1,1.2828428 1.2585786,1.8 1,1.8 1,2.2 1.2585786,2.2 1,2.7171572 1.1414214,3 1.4,2.4828428 1.4,3 1.6,3 1.6,2.4828428 1.8585786,3 2,2.7171572 1.7414214,2.2 2,2.2 2,1.8 1.7414214,1.8 2,1.2828428 1.8585786,1 1.6,1.5171572 1.6,1 1.4,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>'
      )
  makeSymbol('stadium', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <rect id=\"stadium\" x=\"1\" y=\"1\" height=\"2\" width=\"1\" rx=\"25%\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></rect>\n</svg>'
      )
  makeSymbol('line', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <line id=\"line\" x1=\"0\" x2=\"3\" y1=\"2\" y2=\"2\" stroke=\"black\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></line>\n</svg>'
      )
  makeSymbol('polygon', width = 1, height = 2,
             color = 'black', fillColor = 'blue',
             opacity = .9, fillOpacity = .7) %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"polygon\" points=\"1.5,1 1.02447174185242,1.69098300562505 1.20610737385376,2.80901699437495 1.79389262614624,2.80901699437495 1.97552825814758,1.69098300562505 1.5,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>'
      )
  draw_polygon(2) %>%
    testthat::expect_equal("0,0.5 1,0.5 0,0.5")
  makeLegendSymbol(label = '', labelStyle = '', shape = 'rect', width = 1,
                   color = 'black') %>%
    as.character() %>%
    URLdecode() %>%
    testthat::expect_equal(
    '<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="3" height="3">
  <rect id="rect" x="1" y="1" height="1" width="1" stroke="black" fill="black" stroke-opacity="1" fill-opacity="1"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; "></span>')
  makeLegendSymbol(label = '', labelStyle = '', shape = 'rect', width = 1,
                   color = 'black', orientation = 'horizontal') %>%
    as.character() %>%
    URLdecode() %>%
    testthat::expect_equal(
      '<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="3" height="3">
  <rect id="rect" x="1" y="1" height="1" width="1" stroke="black" fill="black" stroke-opacity="1" fill-opacity="1" orientation="horizontal"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; "></span>')
  # test negative sizes
  sizeNumeric(values = 1:4, baseSize = -5) %>%
    testthat::expect_error()
  sizeBreaks(values = 1:4, breaks = 4, baseSize = -5) %>%
    testthat::expect_error()
  sizeBreaks(values = 1:4, breaks = -1, baseSize = 5) %>%
    testthat::expect_error()
  # test simple size outputs
  sizeNumeric(1:4, 5) %>%
    testthat::expect_equal(c(2, 4, 6, 8))
  sizeBreaks(1:4, 4, 5) %>%
    testthat::expect_equal(stats::setNames(c(2, 4, 6, 8), c(1, 2, 3, 4)))
  sizeBreaks(1:4, 1:4, 5) %>%
    testthat::expect_equal(stats::setNames(c(2, 4, 6, 8), c(1, 2, 3, 4)))
  # test size icons args
  pal <- leaflet::colorNumeric('Reds', 1:4)
  makeSizeIcons(values = 1:4, shape = 'notashape',
                color = 'black', baseSize = 5) %>%
    testthat::expect_error()
  makeSizeIcons(values = 1:4, shape = 'rect',
                color = 'black', baseSize = -5) %>%
    testthat::expect_error()
  makeSizeIcons(values = 1:4, shape = 'rect',
                color = 'black', baseSize = 5, opacity = '1') %>%
    testthat::expect_error()
  makeSizeIcons(values = 1:4, shape = c('rect', 'circle'),
                color = 'black', baseSize = 5, opacity = 1) %>%
    testthat::expect_error()
  makeSizeIcons(values = 1:4, shape = 'rect',
                color = 'black', baseSize = 5, opacity = 1,
                strokeWidth = -1) %>%
    testthat::expect_error()
  makeSizeIcons(values = 1:4, shape = 'rect',
                colorValues = 4:1, baseSize = 5, opacity = 1) %>%
    testthat::expect_error()
  makeSizeIcons(values = 1:4, shape = 'rect', baseSize = 5, opacity = 1) %>%
    testthat::expect_error()
  makeSizeIcons(values = 1:4, shape = 'rect', baseSize = 5, opacity = 1,
                color = 'black',
                fillColor = c('black', 'blue')) %>%
    testthat::expect_error()
  # test size icons
  makeSizeIcons(values = 1:4, shape = 'rect', pal = pal,
                color = 'black', baseSize = 5)$iconUrl[[1]] %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="4" height="4">
  <rect id="rect" x="1" y="1" height="2" width="2" stroke="black" fill="#FFF5F0" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>')
  makeSizeIcons(values = 1:4, shape = 'rect', fillColor = 'blue',
                color = 'black', baseSize = 5)$iconUrl[[2]] %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="6" height="6">
  <rect id="rect" x="1" y="1" height="4" width="4" stroke="black" fill="blue" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>')
  makeSizeIcons(values = 1:4, shape = 'rect', colorValues = 4:1, pal = pal,
                baseSize = 5)$iconUrl[[3]] %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="8" height="8">
  <rect id="rect" x="1" y="1" height="6" width="6" stroke="#FDA081" fill="#FDA081" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>')
  makeSizeIcons(values = 1:4, shape = 'rect', fillColor = pal(4:1),
                color = pal(1:4),
                baseSize = 5)$iconUrl[[4]] %>%
    URLdecode() %>%
    testthat::expect_equal(
      'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="10" height="10">
  <rect id="rect" x="1" y="1" height="8" width="8" stroke="#67000D" fill="#FFF5F0" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>')
})

testthat::test_that('Size Legends', {
  mapData <- data.frame(x = 1:2)
  pal <- leaflet::colorNumeric('Reds', mapData[['x']])
  m <- leaflet::leaflet()
  # test args
  m %>%
    addLegendSize(data = mapData, values = ~x) %>%
    testthat::expect_error()
  m %>%
    addLegendSize(data = mapData, values = ~x, pal = pal,
                  fillColor = c('blue', 'red')) %>%
    testthat::expect_error()
  m %>%
    addLegendSize(data = mapData, values = ~x, color = 'black',
                  numberFormat = 'fun') %>%
    testthat::expect_error()
  m %>%
    addLegendSize(data = mapData, values = ~x, color = 'black',
                  shape = 'notashape') %>%
    testthat::expect_error()
  m %>%
    addLegendLine(data = mapData, values = ~x) %>%
    testthat::expect_error()
  m %>%
    addLegendLine(data = mapData, values = ~x) %>%
    testthat::expect_error()
  m %>%
    addLegendLine(data = mapData, values = ~x, color = 'black',
                  numberFormat = 'fun') %>%
    testthat::expect_error()
  # test size legends
  m %>%
    addLegendSize(data = mapData, values = ~x, pal = pal, breaks = 1,
                  color = 'black') %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    URLdecode() %>%
    testthat::expect_equal(
      '<div>
  <img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="8.66666666666667" height="8.66666666666667">
  <rect id="rect" x="1" y="1" height="6.66666666666667" width="6.66666666666667" stroke="black" fill="#FFF5F0" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; margin: 5px; margin-right: 3.33333333333333px; margin-left: 3.33333333333333px" height="6.66666666666667" width="6.66666666666667"/>
  <span style="">1</span>
</div>
<div>
  <img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="15.3333333333333" height="15.3333333333333">
  <rect id="rect" x="1" y="1" height="13.3333333333333" width="13.3333333333333" stroke="black" fill="#67000D" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; margin: 5px; margin-right: 0px; margin-left: 0px" height="13.3333333333333" width="13.3333333333333"/>
  <span style="">2</span>
</div>')
  # test line legends
  m %>%
    addLegendLine(data = mapData, values = ~x, color = 'black', breaks = 1) %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    URLdecode() %>%
    testthat::expect_equal(
      '<div>
  <img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="20" height="6.66666666666667">
  <rect id="rect" x="0" y="0" height="6.66666666666667" width="20" stroke="transparent" fill="black" stroke-opacity="1" fill-opacity="1" stroke-width="0"></rect>
</svg>" style="vertical-align: middle; margin: 5px; margin-right: 0px; margin-left: 0px" height="6.66666666666667" width="20"/>
  <span style="">1</span>
</div>
<div>
  <img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="20" height="13.3333333333333">
  <rect id="rect" x="0" y="0" height="13.3333333333333" width="20" stroke="transparent" fill="black" stroke-opacity="1" fill-opacity="1" stroke-width="0"></rect>
</svg>" style="vertical-align: middle; margin: 5px; margin-right: 0px; margin-left: 0px" height="13.3333333333333" width="20"/>
  <span style="">2</span>
</div>')
})

testthat::test_that('Image Legend', {
  m <- leaflet::leaflet()
  # test addLegendImage Args
  colors <- c('blue', 'red', 'yellow', 'green', 'orange', 'purple')
  leafImg <- system.file(sprintf('img/leaf-%s.png', colors),
                         package = 'leaflegend')
  testthat::expect_error(m %>% addLegendImage(images = leafImg,
                                              labels = colors,
                                              orientation = 'up'))
  testthat::expect_error(m %>% addLegendImage(images = leafImg,
                                              labels = colors,
                                              title = 1:2))
  testthat::expect_error(m %>% addLegendImage(images = seq_along(colors),
                                              labels = colors))
  testthat::expect_error(m %>% addLegendImage(images = leafImg[1:2],
                                              labels = colors))
  testthat::expect_error(m %>% addLegendImage(images = leafImg,
                                              labels = colors[1:2]))
  testthat::expect_error(m %>% addLegendImage(images = leafImg,
                                              labels = colors,
                                              width = -1))
  testthat::expect_error(m %>% addLegendImage(images = leafImg,
                                              labels = colors,
                                              height = -1))
  # test addLegendImage legends
  rectImg <- system.file('img/rect.png',
                         package = 'leaflegend')
  m %>% addLegendImage(images = rectImg, labels = 'rect',
                       orientation = 'horizontal') %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    testthat::expect_equal(
      '<div>\n  <strong></strong>\n</div>\n<span>\n  <img src=\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABmJLR0QA/wD/AP+gvaeTAAAAKUlEQVQokWNkYGBwgGJiwAEWqOJ6IjUwMBGrcFTD4NLAwsDAcIAE9QcAil0DmynVMvUAAAAASUVORK5CYII=\" style=\"vertical-align: middle; margin: 5px; margin-right: 0px; margin-left: 0px\" height=\"20\" width=\"20\"/>\n  <span style=\"font-size: 24px; vertical-align: middle;\">rect</span>\n</span>'
      )
  m %>% addLegendImage(
    images = makeSymbol('rect', width = 10, color = 'black'),
    labels = 'rect') %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    URLdecode() %>%
    testthat::expect_equal(
      '<div>
  <strong></strong>
</div>
<div>
  <img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="12" height="12">
  <rect id="rect" x="1" y="1" height="10" width="10" stroke="black" fill="black" stroke-opacity="1" fill-opacity="1"></rect>
</svg>" style="vertical-align: middle; margin: 5px; margin-right: 0px; margin-left: 0px" height="20" width="20"/>
  <span style="font-size: 24px; vertical-align: middle;">rect</span>
</div>'
    )
})

testthat::test_that('Numeric Legend', {
  mapData <- data.frame(x = 1:2)
  m <- leaflet::leaflet(mapData)
  pal <- leaflet::colorNumeric('Reds', mapData[['x']])
  # test args
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         orientation = 'up') %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         title = 1:2) %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         shape = 'circle') %>%
    testthat::expect_error()
  # is this test necessary
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         width = -1) %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         height = -1) %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         numberFormat = 'fun') %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         bins = -1) %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         decreasing = NA) %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         decreasing = NULL) %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         decreasing = 'TRUE') %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         tickLength = -1) %>%
    testthat::expect_error()
  m %>% addLegendNumeric(pal = pal,
                         values = ~x,
                         tickWidth = -1) %>%
    testthat::expect_error()
  # test results
  numVert <- m %>% addLegendNumeric(pal = pal, values = ~x)
  numHori <- m %>% addLegendNumeric(pal = pal, values = ~x,
                                    orientation = 'horizontal')
#   testthat::expect_equal(numVert[["x"]][["calls"]][[1]][["args"]][[1]],
#     expected = '<div style="display: flex;">
#   <div style="margin-right: 5px">
#     <svg width="24" height="100">
#       <defs>
#         <linearGradient id="gradient-x" x1="0" y1="0" x2="0" y2="1">
#           <stop offset="0.000000%" stop-color="#FFF5F0"></stop>
#           <stop offset="20.000000%" stop-color="#FDCAB4"></stop>
#           <stop offset="40.000000%" stop-color="#FC8A6A"></stop>
#           <stop offset="60.000000%" stop-color="#F24632"></stop>
#           <stop offset="80.000000%" stop-color="#BC141A"></stop>
#           <stop offset="100.000000%" stop-color="#67000D"></stop>
#         </linearGradient>
#       </defs>
#       <g>
#         <rect height="100" width="20" x="0" rx="0%" fill-opacity="1" fill="url(#gradient-x)"></rect>
#       </g>
#       <line x1="20" x2="24" y1="20" y2="20" stroke-width="1" stroke="black"></line>
#       <line x1="20" x2="24" y1="40" y2="40" stroke-width="1" stroke="black"></line>
#       <line x1="20" x2="24" y1="60" y2="60" stroke-width="1" stroke="black"></line>
#       <line x1="20" x2="24" y1="80" y2="80" stroke-width="1" stroke="black"></line>
#     </svg>
#   </div>
#   <div style="width: 16.7441860465116px; height: 100px; position: relative; " class="container">
#     <div style="position:absolute; left:0px; top: 10.7906976744186px;">1.2</div>
#     <div style="position:absolute; left:0px; top: 30.7906976744186px;">1.4</div>
#     <div style="position:absolute; left:0px; top: 50.7906976744186px;">1.6</div>
#     <div style="position:absolute; left:0px; top: 70.7906976744186px;">1.8</div>
#   </div>
#   <div style="width: 8px; position: relative;"></div>
# </div>')
#   testthat::expect_equal(numHori[["x"]][["calls"]][[1]][["args"]][[1]],
#                          '<div style="margin-right: 3.34883720930233px; margin-left: 3.34883720930233px">
#   <svg width="20" height="104">
#     <defs>
#       <linearGradient id="gradient-x" x1="0" y1="0" x2="1" y2="0">
#         <stop offset="0.000000%" stop-color="#FFF5F0"></stop>
#         <stop offset="20.000000%" stop-color="#FDCAB4"></stop>
#         <stop offset="40.000000%" stop-color="#FC8A6A"></stop>
#         <stop offset="60.000000%" stop-color="#F24632"></stop>
#         <stop offset="80.000000%" stop-color="#BC141A"></stop>
#         <stop offset="100.000000%" stop-color="#67000D"></stop>
#       </linearGradient>
#     </defs>
#     <g>
#       <rect height="100" width="20" x="0" rx="0%" fill-opacity="1" fill="url(#gradient-x)"></rect>
#     </g>
#     <line x1="0" x2="0" y1="100" y2="104" stroke-width="1" stroke="black"></line>
#     <line x1="20" x2="20" y1="100" y2="104" stroke-width="1" stroke="black"></line>
#   </svg>
# </div>
# <div style="width: 100%; height: 1rem; position: relative; ">
#   <div style="position:absolute; left:0px; top: 0px;">1</div>
#   <div style="position:absolute; left:20px; top: 0px;">2</div>
# </div>')

})

testthat::test_that('Categorical Legends', {
  # test Quantile args
  mapData <- data.frame(x = 1:10,
                        y = rep(LETTERS[1:2], 5))
  m <- leaflet::leaflet(mapData)
  palQuantile <- leaflet::colorQuantile('Reds', mapData[['x']], n = 2)
  palBins <- leaflet::colorBin('Blues', mapData[['x']], bins = 2)
  palFactor <- leaflet::colorFactor('viridis', mapData[['y']])
  # test Quantile args
  m %>% addLegendQuantile(pal = palQuantile,
                          values = ~x,
                          orientation = 'up') %>%
    testthat::expect_error()
  m %>% addLegendQuantile(pal = palQuantile,
                          values = ~x,
                          title = 1:2) %>%
    testthat::expect_error()
  m %>% addLegendQuantile(pal = palQuantile,
                          values = ~x,
                          shape = 'notashape') %>%
    testthat::expect_error()
  m %>% addLegendQuantile(pal = palQuantile,
                          values = ~x,
                          width = -1) %>%
    testthat::expect_error()
  m %>% addLegendQuantile(pal = palQuantile,
                          values = ~x,
                          height = -1) %>%
    testthat::expect_error()
  m %>% addLegendQuantile(pal = palQuantile,
                          values = ~x,
                          numberFormat = 'fun') %>%
    testthat::expect_error()
  # test Bin args
  m %>% addLegendBin(pal = palBin,
                     values = ~x,
                     orientation = 'up') %>%
    testthat::expect_error()
  m %>% addLegendBin(pal = palBin,
                     values = ~x,
                     title = 1:2) %>%
    testthat::expect_error()
  m %>% addLegendBin(pal = palBin,
                     values = ~x,
                     shape = 'notashape') %>%
    testthat::expect_error()
  m %>% addLegendBin(pal = palBin,
                     values = ~x,
                     width = -1) %>%
    testthat::expect_error()
  m %>% addLegendBin(pal = palBin,
                     values = ~x,
                     height = -1) %>%
    testthat::expect_error()
  m %>% addLegendBin(pal = palBin,
                     values = ~x,
                     numberFormat = 'fun') %>%
    testthat::expect_error()
  # test Factor args
  m %>% addLegendFactor(pal = palFactor,
                        values = ~y,
                        orientation = 'up') %>%
    testthat::expect_error()
  m %>% addLegendFactor(pal = palFactor,
                        values = ~y,
                        title = 1:2) %>%
    testthat::expect_error()
  m %>% addLegendFactor(pal = palFactor,
                        values = ~y,
                        shape = 'notashape') %>%
    testthat::expect_error()
  m %>% addLegendFactor(pal = palFactor,
                        values = ~y,
                        width = -1) %>%
    testthat::expect_error()
  m %>% addLegendFactor(pal = pal,
                        values = ~y,
                        height = -1) %>%
    testthat::expect_error()
  m %>% addLegendFactor(pal = palFactor,
                        values = ~y,
                        numberFormat = 'fun') %>%
    testthat::expect_error()
  # test results
  m %>% addLegendQuantile(pal = palQuantile,
                          values = ~x) %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    strsplit('\n') %>%
    getElement(1) %>%
    lapply(function(x) {
      if ( substring(x, 2,4) == 'img' ) {
        URLdecode(x)
      } else {
        x
      }
    }
    ) %>%
    paste0(collapse = '\n') %>%
    testthat::expect_equal(
      '<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="26" height="26">
  <rect id="rect" x="1" y="1" height="24" width="24" stroke="#FEE0D2" fill="#FEE0D2" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; ">  0% -  50% (1 - 6)</span>
<br/>
<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="26" height="26">
  <rect id="rect" x="1" y="1" height="24" width="24" stroke="#DE2D26" fill="#DE2D26" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; "> 50% - 100% (6 - 10)</span>
<br/>')
  m %>% addLegendBin(pal = palBins,
                     values = ~x) %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    URLdecode() %>%
    testthat::expect_equal(
      '<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="26" height="26">
  <rect id="rect" x="1" y="1" height="24" width="24" stroke="#DEEBF7" fill="#DEEBF7" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; "> 0 - 5</span>
<br/>
<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="26" height="26">
  <rect id="rect" x="1" y="1" height="24" width="24" stroke="#3182BD" fill="#3182BD" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; "> 5 - 10</span>
<br/>')
  m %>% addLegendFactor(pal = palFactor,
                        values = ~y) %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    URLdecode() %>%
    testthat::expect_equal(
      '<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="26" height="26">
  <rect id="rect" x="1" y="1" height="24" width="24" stroke="#440154" fill="#440154" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; "> A</span>
<br/>
<img src="data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="26" height="26">
  <rect id="rect" x="1" y="1" height="24" width="24" stroke="#FDE725" fill="#FDE725" stroke-opacity="1" fill-opacity="1" stroke-width="1"></rect>
</svg>" style="vertical-align: middle; padding: 1px;"/>
<span style="vertical-align: middle; padding: 1px; "> B</span>
<br/>')
})

testthat::test_that('Awesome Legends', {
  m <- leaflet::leaflet()
  iconSet <- leaflet::awesomeIconList(
    'home' = leaflet::makeAwesomeIcon()
  )
  # test args
  m %>%
    addLegendAwesomeIcon(iconSet = list()) %>%
    testthat::expect_error()
  m %>%
    addLegendAwesomeIcon(
      iconSet = leaflet::awesomeIconList(leaflet::makeAwesomeIcon())) %>%
    testthat::expect_error()
  m %>%
    addLegendAwesomeIcon(iconSet = iconSet, marker = NA) %>%
    testthat::expect_error()
  # test results
  m %>%
    addLegendAwesomeIcon(iconSet = iconSet) %>%
    getElement('dependencies') %>%
    vapply(getElement, 'name', FUN.VALUE = character(1)) %>%
    testthat::expect_equal(c('leaflet-awesomemarkers', 'bootstrap'))
  m %>%
    addLegendAwesomeIcon(iconSet = iconSet) %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    testthat::expect_equal(
      '<div>
  <div style="vertical-align: middle; display: inline-block; position: relative;" class="awesome-marker-icon-blue awesome-marker ">
    <i class="glyphicon glyphicon-home " style="color: white; ; margin-right: 0px"></i>
  </div>
  <span style="">home</span>
</div>')
  m %>%
    addLegendAwesomeIcon(iconSet = iconSet, orientation = 'horizontal',
                         title = 'Icons') %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(1) %>%
    testthat::expect_equal(
      '<div>
  <strong>Icons</strong>
</div>
<span>
  <div style="vertical-align: middle; display: inline-block; position: relative;" class="awesome-marker-icon-blue awesome-marker ">
    <i class="glyphicon glyphicon-home " style="color: white; ; margin-right: 0px"></i>
  </div>
  <span style="">home</span>
</span>'
    )
})

testthat::test_that('Helper Functions', {
  parseValues(1:2) %>%
    testthat::expect_equal(1:2)
  m <- leaflet::leaflet()
  m %>%
    leaflegendAddControl(html = '<p>Test</p>', group = 'Test') %>%
    testthat::expect_error()
  m %>%
    leaflegendAddControl(className = '', group = 'Test') %>%
    testthat::expect_error()
  m %>%
    leaflegendAddControl(html = '',className = '', group = 'Test') %>%
    getElement(1) %>%
    getElement('calls') %>%
    getElement(1) %>%
    getElement('args') %>%
    getElement(4) %>%
    testthat::expect_equal(' leaflegend-group-Test')
  leafletAwesomeMarkersDependencies() %>%
    testthat::expect_equal(leaflet:::leafletAwesomeMarkersDependencies())
  leafletAmFontAwesomeDependencies() %>%
    testthat::expect_equal(leaflet:::leafletAmFontAwesomeDependencies())
  leafletAmIonIconDependencies() %>%
    testthat::expect_equal(leaflet:::leafletAmIonIconDependencies())
  leafletAmBootstrapDependencies() %>%
    testthat::expect_equal(leaflet:::leafletAmBootstrapDependencies())
})
