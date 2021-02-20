#' Add a Legend with Images
#'
#' Creates a legend with images that are embedded into a 'leaflet' map so that
#' images do not need to be packaged when saving a 'leaflet' map as HTML. Full
#' control over the label and title style. The 'leaflet' map is passed through
#' and the output is a control so that legend is fully integrated with other
#' functionalities.
#'
#' @param map
#'
#' a map widget object created from 'leaflet'
#'
#' @param images
#'
#' path to the image file
#'
#' @param labels
#'
#' labels for each image
#'
#' @param title
#'
#' the legend title, pass in HTML to style
#'
#' @param labelStyle
#'
#' character string of style argument for HTML text
#'
#' @param orientation
#'
#' stack the legend items vertically or horizontally
#'
#' @param width
#'
#' in pixels
#'
#' @param height
#'
#' in pixels
#'
#' @param ...
#'
#' arguments to pass to \link[leaflet]{addControl}
#'
#' @return
#'
#' an object from \link[leaflet]{addControl}
#'
#' @export
#'
#' @examples
#'
#' library(leaflet)
#' data(quakes)
#'
#' quakes1 <- quakes[1:10,]
#'
#' leafIcons <- icons(
#'   iconUrl = ifelse(quakes1$mag < 4.6,
#'                    "http://leafletjs.com/examples/custom-icons/leaf-green.png",
#'                    "http://leafletjs.com/examples/custom-icons/leaf-red.png"
#'   ),
#'   iconWidth = 38, iconHeight = 95,
#'   iconAnchorX = 22, iconAnchorY = 94,
#'   shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
#'   shadowWidth = 50, shadowHeight = 64,
#'   shadowAnchorX = 4, shadowAnchorY = 62
#' )
#'
#' leaflet(data = quakes1) %>% addTiles() %>%
#'   addMarkers(~long, ~lat, icon = leafIcons) %>%
#'   addLegendImage(images = c("http://leafletjs.com/examples/custom-icons/leaf-green.png",
#'                             "http://leafletjs.com/examples/custom-icons/leaf-red.png"),
#'                  labels = c('Green', 'Red'),width = 38, height = 95,
#'                  title = htmltools::tags$div('Leaf',
#'                  style = 'font-size: 24px; text-align: center;'),
#'                  position = 'topright')
addLegendImage <- function(map,
                           images,
                           labels,
                           title = '',
                           labelStyle = 'font-size: 24px; vertical-align: middle;',
                           orientation = c('vertical', 'horizontal'),
                           width = 20,
                           height = 20,
                           ...) {
  stopifnot(length(images) == length(labels))
  orientation <- match.arg(orientation)
  if ( orientation == 'vertical' ) {
    htmlTag <- htmltools::tags$div
  } else {
    htmlTag <- htmltools::tags$span
  }
  htmlElements <- Map(
    img = images,
    label = labels,
    htmlTag = list(htmlTag),
    width = width,
    height = height,
    f =
      function(img, label, htmlTag, height, width) {
        if ( inherits(img, 'svgURI') ) {
          imgTag <- htmltools::tags$img(
            src = img,
            style = 'vertical-align: middle; padding: 5px;',
            height = height,
            width = width
          )
        } else {
          fileExt <- tolower(sub('.+(\\.)([a-zA-Z]+)', '\\2', img))
          stopifnot(fileExt %in% c('png', 'jpg', 'jpeg'))
          imgTag <- htmltools::tags$img(
            src = sprintf(
              'data:image/%s;base64,%s',
              fileExt,
              base64enc::base64encode(img)
            ),
            style = 'vertical-align: middle; padding: 5px;',
            height = height,
            width = width
          )
        }
        htmlTag(imgTag, htmltools::tags$span(label, style = labelStyle))
      }
  )
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(htmltools::tags$strong(title))), after = 0)
  }
  leaflet::addControl(map, html = htmltools::tagList(htmlElements), ...)
}

#' Create an SVG tag for the symbol
#'
#'
#' @param shape
#'
#' the desired shape of the symbol
#'
#' @param width
#'
#' in pixels
#'
#' @param height
#'
#' in pixels
#'
#' @param color
#'
#' color of the symbol
#'
#' @param fillColor
#'
#' fill color of symbol
#'
#' @param opacity
#'
#' opacity of color
#'
#' @param fillOpacity
#'
#' opacity of fillColor
#'
#' @param ...
#'
#' arguments to be passed to svg shape tag
#'
#' @return
#'
#' HTML svg element
#'
#' @export
#'
makeSymbol <- function(shape, width, height, color, fillColor = color,
                       opacity = 1, fillOpacity = opacity, ...) {
  strokewidth <- 0
  if ( 'stroke-width' %in% names(list(...)) ) {
    strokewidth <- list(...)[['stroke-width']]
  }
  shapeTag <- switch(
    shape,
    'rect' = htmltools::tags$rect(
      id = 'rect',
      x = strokewidth,
      y = strokewidth,
      height = height,
      width = width,
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'circle' = htmltools::tags$circle(
      id = 'circle',
      cx = height / 2 + strokewidth,
      cy = height / 2 + strokewidth,
      r = height / 2,
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'triangle' = htmltools::tags$polygon(
      id = 'triangle',
      points = sprintf('%s,%s %s,%s %s,%s',
                       strokewidth,
                       height + strokewidth,
                       width + strokewidth,
                       height + strokewidth,
                       width / 2  + strokewidth,
                       strokewidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'plus' = htmltools::tags$polygon(
      id = 'plus',
      points = draw_plus(width = width, height = height, offset = strokewidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'cross' = htmltools::tags$polygon(
      id = 'cross',
      points = draw_cross(width = width, height = height, offset = strokewidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'diamond' = htmltools::tags$polygon(
      id = 'diamond',
      points = draw_diamond(width = width, height = height, offset = strokewidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'star' = htmltools::tags$path(
      id = 'star',
      d = sprintf('M %s z M %s z',
                  draw_plus(width = width, height = height, offset = strokewidth),
                  draw_cross(width = width, height = height, offset = strokewidth)),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'stadium' = htmltools::tags$rect(
      id = 'stadium',
      x = strokewidth,
      y = strokewidth,
      height = height,
      width = width,
      rx = "25%",
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    stop('Invalid shape argument.')
  )
  svgURI <-
    sprintf('data:image/svg+xml,%s',
            utils::URLencode(as.character(
              htmltools::tags$svg(
                xmlns = "http://www.w3.org/2000/svg",
                version = "1.1",
                width = width + strokewidth * 2,
                height = height + strokewidth * 2,
                shapeTag
              )
            ), reserved = TRUE))
  structure(svgURI, class = c(class(svgURI), 'svgURI'))
}

makeLegendSymbol <- function(label, labelStyle, ...) {
  shapeTag <- makeSymbol(...)
  htmltools::tagList(
    htmltools::tags$img(src = shapeTag, style = "vertical-align: middle; padding: 1px;"),
    htmltools::tags$span(label, style = sprintf("vertical-align: middle; padding: 1px; %s", labelStyle))
  )
}

draw_plus <- function(width, height, offset = 0) {
  x <- width * c(rep(c(.4, 0, .4, .6, 1, .6), each = 2), .4) + offset
  y <- height * c(0, rep(c(.4, .6, 1, .6, .4, 0), each = 2)) + offset
  paste(x, y, sep = ',', collapse = ' ')
}

draw_diamond <- function(width, height, offset = 0) {
  x <- width * c( .5, 0, .5, 1, .5) + offset
  y <- height * c(0, .5, 1, .5, 0) + offset
  paste(x, y, sep = ',', collapse = ' ')
}

draw_cross <- function(width, height, offset = 0) {
  a <- sqrt(2) / 10
  x <- width *  c(a, 0, .5 - a, 0, a, .5, 1 - a, 1, .5 + a, 1, 1 - a, .5, a) + offset
  y <- height * c(0, a, .5, 1 - a, 1, .5 + a, 1, 1 - a, .5, a, 0, .5 - a, 0) + offset
  paste(x, y, sep = ',', collapse = ' ')
}

#' Add Customizable Color Legends to a 'leaflet' map widget
#'
#' Functions for more control over the styling of 'leaflet' legends. The 'leaflet'
#' map is passed through and the output is a 'leaflet' control so that
#' the legends are integrated with other functionality of the API. Style
#' the text of the labels, the symbols used, orientation of the legend items,
#' and sizing of all elements.
#'
#' @param map
#'
#' a map widget object created from 'leaflet'
#'
#' @param pal
#'
#' the color palette function, generated from \link[leaflet]{colorNumeric}
#'
#' @param values
#'
#' the values used to generate colors from the palette function
#'
#' @param bins
#'
#' an approximate number of tick-marks on the color gradient for the colorNumeric palette
#'
#' @param title
#'
#' the legend title, pass in HTML to style
#'
#' @param shape
#'
#' shape of the color symbols
#'
#' @param orientation
#'
#' stack the legend items vertically or horizontally
#'
#' @param width
#'
#' in pixels
#'
#' @param height
#'
#' in pixels
#'
#' @param numberFormat
#'
#' formatting functions for numbers that are displayed e.g. format, prettyNum
#'
#' @param labelStyle
#'
#' character string of style argument for HTML text
#'
#' @param tickLength
#'
#' in pixels
#'
#' @param tickWidth
#'
#' in pixels
#'
#' @param decreasing
#'
#' order of numbers in the legend
#'
#' @param opacity
#'
#' opacity of the legend items
#'
#' @param fillOpacity
#'
#' fill opacity of the legend items
#'
#' @param ...
#'
#' arguments to pass to \link[leaflet]{addControl}
#'
#' @export
#'
#' @return
#'
#' an object from \link[leaflet]{addControl}
#'
#' @name addLeafLegends
#'
#' @examples
#' library(leaflet)
#'
#' data(quakes)
#'
#' # Numeric Legend
#'
#' numPal <- colorNumeric('viridis', quakes$depth)
#' leaflet() %>%
#'   addTiles() %>%
#'   addLegendNumeric(
#'     pal = numPal,
#'     values = quakes$depth,
#'     position = 'topright',
#'     title = 'addLegendNumeric (Horizontal)',
#'     orientation = 'horizontal',
#'     shape = 'rect',
#'     decreasing = FALSE,
#'     height = 20,
#'     width = 100
#'   ) %>%
#'   addLegendNumeric(
#'     pal = numPal,
#'     values = quakes$depth,
#'     position = 'topright',
#'     title = htmltools::tags$div('addLegendNumeric (Decreasing)',
#'     style = 'font-size: 24px; text-align: center; margin-bottom: 5px;'),
#'     orientation = 'vertical',
#'     shape = 'stadium',
#'     decreasing = TRUE,
#'     height = 100,
#'     width = 20
#'   ) %>%
#'   addLegend(pal = numPal, values = quakes$depth, title = 'addLegend')
#'
#' # Quantile Legend
#' # defaults to adding quantile numeric break points
#'
#' quantPal <- colorQuantile('viridis', quakes$mag, n = 5)
#' leaflet() %>%
#'   addTiles() %>%
#'   addCircleMarkers(data = quakes,
#'                    lat = ~lat,
#'                    lng = ~long,
#'                    color = ~quantPal(mag),
#'                    opacity = 1,
#'                    fillOpacity = 1
#'   ) %>%
#'   addLegendQuantile(pal = quantPal,
#'                     values = quakes$mag,
#'                     position = 'topright',
#'                     title = 'addLegendQuantile',
#'                     numberFormat = function(x) {prettyNum(x, big.mark = ',',
#'                     scientific = FALSE, digits = 2)},
#'                     shape = 'circle') %>%
#'   addLegendQuantile(pal = quantPal,
#'                     values = quakes$mag,
#'                     position = 'topright',
#'                     title = htmltools::tags$div('addLegendQuantile',
#'                                                 htmltools::tags$br(),
#'                                                 '(Omit Numbers)'),
#'                     numberFormat = NULL,
#'                     shape = 'circle') %>%
#'   addLegend(pal = quantPal, values = quakes$mag, title = 'addLegend')
#'
#' # Factor Legend
#' # Style the title with html tags, several shapes are supported drawn with svg
#'
#' quakes[['group']] <- sample(c('A', 'B', 'C'), nrow(quakes), replace = TRUE)
#' factorPal <- colorFactor('Dark2', quakes$group)
#' leaflet() %>%
#'   addTiles() %>%
#'   addCircleMarkers(
#'     data = quakes,
#'     lat = ~ lat,
#'     lng = ~ long,
#'     color = ~ factorPal(group),
#'     opacity = 1,
#'     fillOpacity = 1
#'   ) %>%
#'   addLegendFactor(
#'     pal = factorPal,
#'     title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px; color: red;'),
#'     values = quakes$group,
#'     position = 'topright',
#'     shape = 'triangle',
#'     width = 50,
#'     height = 50
#'   ) %>%
#'   addLegend(pal = factorPal,
#'             values = quakes$group,
#'             title = 'addLegend')
#'
#' # Bin Legend
#' # Restyle the text of the labels, change the legend item orientation
#'
#' binPal <- colorBin('Set1', quakes$mag)
#' leaflet() %>%
#'   addTiles() %>%
#'   addCircleMarkers(
#'     data = quakes,
#'     lat = ~ lat,
#'     lng = ~ long,
#'     color = ~ binPal(mag),
#'     opacity = 1,
#'     fillOpacity = 1
#'   ) %>%
#'   addLegendBin(
#'     pal = binPal,
#'     values = quakes$mag,
#'     position = 'topright',
#'     title = 'addLegendBin',
#'     labelStyle = 'font-size: 18px; font-weight: bold;',
#'     orientation = 'horizontal'
#'   ) %>%
#'   addLegend(pal = binPal,
#'             values = quakes$mag,
#'             title = 'addLegend')
addLegendNumeric <- function(map,
                             pal,
                             values,
                             title = NULL,
                             #labelStyle = 'font-size: 24px; vertical-align: middle;',
                             shape = c('rect', 'stadium'),
                             orientation = c('vertical', 'horizontal'),
                             width = 20,
                             height = 100,
                             bins = 7,
                             numberFormat = function(x) {prettyNum(x, format = 'f', big.mark = ',', scientific = FALSE)},
                             tickLength = 4,
                             tickWidth = 1,
                             decreasing = FALSE,
                             fillOpacity = 1,
                             ...) {
  stopifnot( attr(pal, 'colorType') == 'numeric' )
  rng <- range(values, na.rm = TRUE)
  bins <- bins
  breaks <- pretty(values, bins)
  if ( breaks[1] < rng[1] ) {
    breaks[1] <- rng[1]
  }
  if ( breaks[length(breaks)] > rng[2] ) {
    breaks[length(breaks)] <- rng[2]
  }
  colors <- pal(breaks)
  scaledbreaks <- (breaks - rng[1]) / (rng[2] - rng[1])
  offsets <- sprintf('%f%%', scaledbreaks * 100)
  invisible(lapply(c('x1', 'y1', 'x2', 'y2'), assign, 0, pos = environment()))
  orientation <- match.arg(orientation)
  vertical <- orientation =='vertical'
  outer <- c(1, length(breaks))
  if ( vertical ) {
    labels <- breaks[-outer]
  } else {
    labels <- breaks[outer]
  }
  if ( decreasing ) {
    labels <- rev(labels)
  }
  if ( vertical & decreasing ) {
    y1 <- 1
  } else if ( vertical & !decreasing ) {
    y2 <- 1
  } else if ( !vertical & decreasing ) {
    x1 <- 1
  } else {
    x2 <- 1
  }
  labels <- numberFormat(labels)
  textSpace <- max(graphics::strwidth(labels, units = 'inches', cex = 1.5)) * 72
  padLabel <- 5
  if ( vertical ) {
    svgwidth <- width + tickLength + padLabel + textSpace
    svgheight <- height
    rectx <- 0
    linex1 <- width
    linex2 <- width + tickLength
    liney1 <- scaledbreaks[-outer] * height
    liney2 <- scaledbreaks[-outer] * height
    textx <- 0
    texty <- scaledbreaks[-outer] * height
    textdx <- width + tickLength + padLabel
    textdy <- '.5ex'
    textanchor <- 'start'
  } else {
    svgwidth <- width + textSpace
    svgheight <- height + tickLength + padLabel * 3
    rectx <- textSpace / 2
    linex1 <- scaledbreaks[outer] * width + rectx + .5 * c(1, -1)
    linex2 <- scaledbreaks[outer] * width + rectx + .5 * c(1, -1)
    liney1 <- height
    liney2 <- height + tickLength
    textx <- scaledbreaks[outer] * width + rectx
    texty <- 0
    textdx <- '.5ex'
    textdy <- height + tickLength + padLabel * 3
    textanchor <- 'middle'
  }
  if ( shape == 'rect' ) {
    rectround <- list(rx = '0%')
  } else if ( shape == 'stadium' & vertical ) {
    rectround <- list(rx = '5%')
  } else {
    rectround <- list(ry = '10%')
  }
  id <- paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = '')
  htmlElements <- list(htmltools::tags$svg(width = svgwidth,
                                           height = svgheight,
                           htmltools::tags$def(
                             htmltools::tags$linearGradient(
                               id = id,
                               x1 = x1, y1 = y1, x2 = x2, y2 = y2,
                               htmltools::tagList(Map(htmltools::tags$stop,
                                                      offset = offsets,
                                                      'stop-color' = colors))
                             )
                           ),
                           htmltools::tags$g(
                             do.call(htmltools::tags$rect,
                                     c(height = height,
                                       width = width,
                                       x = rectx,
                                       rectround,
                                       'fill-opacity' = fillOpacity,
                                       fill = sprintf('url(#%s)', id)))
                           ),
                           Map(htmltools::tags$line,
                               x1 = linex1,
                               x2 = linex2,
                               y1 = liney1,
                               y2 = liney2,
                               'stroke-width' = tickWidth,
                               stroke = 'black'
                           ),
                           Map(htmltools::tags$text,
                               #style = labelStyle,
                               labels,
                               dx = textdx,
                               dy = textdy,
                               x = textx,
                               y = texty,
                               'text-anchor' = textanchor
                           )
  )
  )
  if ( !is.null(title) ) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(htmltools::tags$strong(title))), after = 0)
  }
  leaflet::addControl(map, html = htmltools::tagList(htmlElements), ...)
}

#' @export
#'
#' @rdname addLeafLegends
#'
addLegendQuantile <- function(map,
                              pal,
                              values,
                              title = NULL,
                              labelStyle = '',
                              shape = c('rect', 'circle', 'triangle', 'plus', 'cross', 'diamond', 'star', 'stadium'),
                              orientation = c('vertical', 'horizontal'),
                              width = 24,
                              height = 24,
                              numberFormat = function(x) {prettyNum(x, big.mark = ',', scientific = FALSE, digits = 1)},
                              opacity = 1,
                              fillOpacity = opacity,
                              ...) {
  stopifnot( attr(pal, 'colorType') == 'quantile' )
  shape <- match.arg(shape)
  probs <- attr(pal, 'colorArgs')[['probs']]
  if ( is.null(numberFormat) ) {
    labels <- sprintf(' %3.0f%% - %3.0f%%',
                      probs[-length(probs)] * 100,
                      probs[-1] * 100)

  } else {
    breaks <- stats::quantile(x = values, probs = probs, na.rm = TRUE)
    labels <- numberFormat(breaks)
    labels <- sprintf('%3.0f%% - %3.0f%% (%s - %s)',
                    probs[-length(probs)] * 100,
                    probs[-1] * 100,
                    labels[-length(labels)],
                    labels[-1])
  }
  colors <- unique(pal(sort(values)))
  htmlElements <- Map(
    f = makeLegendSymbol,
    shape = shape,
    label = labels,
    color = colors,
    labelStyle = labelStyle,
    height = height,
    width = width,
    opacity = opacity,
    fillOpacity = fillOpacity,
    'stroke-width' = 1
  )
  orientation <- match.arg(orientation)
  if ( orientation == 'vertical' ) {
    htmlElements <- lapply(htmlElements, htmltools::tagList, htmltools::tags$br())
  }
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(htmltools::tags$strong(title))), after = 0)
  }
  leaflet::addControl(map, html = htmltools::tagList(htmlElements), ...)
}

#' @export
#'
#' @rdname addLeafLegends
#'
addLegendBin <- function(map,
                         pal,
                         values,
                         title = NULL,
                         labelStyle = '',
                         shape = c('rect', 'circle', 'triangle', 'plus', 'cross', 'diamond', 'star', 'stadium'),
                         orientation = c('vertical', 'horizontal'),
                         width = 24,
                         height = 24,
                         opacity = 1,
                         fillOpacity = opacity,
                         ...) {
  stopifnot( attr(pal, 'colorType') == 'bin' )
  shape <- match.arg(shape)
  bins <- prettyNum(attr(pal, 'colorArgs')[['bins']], format = 'f', big.mark = ',', scientific = FALSE)
  labels <- sprintf(' %s - %s', bins[-length(bins)], bins[-1])
  colors <- unique(pal(sort(values)))
  htmlElements <- Map(f = makeLegendSymbol,
      shape = shape,
      label = labels,
      color = colors,
      labelStyle = labelStyle,
      height = height,
      width = width,
      opacity = opacity,
      fillOpacity = fillOpacity,
      'stroke-width' = 1)
  orientation <- match.arg(orientation)
  if ( orientation == 'vertical' ) {
    htmlElements <- lapply(htmlElements, htmltools::tagList, htmltools::tags$br())
  }
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(htmltools::tags$strong(title))), after = 0)
  }
  leaflet::addControl(map, html = htmltools::tagList(htmlElements), ...)
}

#' @export
#'
#' @rdname addLeafLegends
#'
addLegendFactor <- function(map,
                            pal,
                            values,
                            title = NULL,
                            labelStyle = '',
                            shape = c('rect', 'circle', 'triangle', 'plus', 'cross', 'diamond', 'star', 'stadium'),
                            orientation = c('vertical', 'horizontal'),
                            width = 24,
                            height = 24,
                            opacity = 1,
                            fillOpacity = opacity,
                            ...) {
  stopifnot( attr(pal, 'colorType') == 'factor' )
  shape <- match.arg(shape)
  labels <- sprintf(' %s', sort(unique(values)))
  colors <- pal(sort(unique(values)))
  htmlElements <- Map(f = makeLegendSymbol,
      shape = shape,
      label = labels,
      color = colors,
      labelStyle = labelStyle,
      height = height,
      width = width,
      opacity = opacity,
      fillOpacity = fillOpacity,
      'stroke-width' = 1)
  orientation <- match.arg(orientation)
  if ( orientation == 'vertical' ) {
    htmlElements <- lapply(htmlElements, htmltools::tagList, htmltools::tags$br())
  }
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(htmltools::tags$strong(title))), after = 0)
  }
  leaflet::addControl(map, html = htmltools::tagList(htmlElements), ...)
}

addLegendSize <- function(map,
                          pal,
                          values,
                          title = NULL,
                          labelStyle = '',
                          shape = c('rect', 'circle', 'triangle', 'plus', 'cross', 'diamond', 'star', 'stadium'),
                          orientation = c('vertical', 'horizontal'),
                          opacity = 1,
                          fillOpacity = opacity,
                          breaks = 5,
                          baseSize = 10,
                          color,
                          ...) {
  shape <- match.arg(shape)
  sizes <- sizeNumeric(values, breaks, baseSize)
  if ( missing(color) ) {
    colors <- pal(as.numeric(names(sizes)))
  } else {
    colors <- color
  }
  symbols <- Map(makeSymbol,
                 shape = shape,
                 width = sizes,
                 height = sizes,
                 color = colors,
                 fillColor = colors,
                 opacity = opacity,
                 fillOpacity = fillOpacity)
  addLegendImage(map, images = symbols, labels = names(sizes),
                 title = title, labelStyle = labelStyle,
                 orientation = orientation, width = sizes, height = sizes, ...)

}

sizeNumeric <- function(values, breaks, baseSize) {
  if ( length(breaks) == 1 ) {
    breaks <- pretty(values, breaks)
  }
  sizes <- breaks / mean(breaks) * baseSize
  setNames(sizes, breaks)[breaks > 0 & breaks <= max(values)]
}
