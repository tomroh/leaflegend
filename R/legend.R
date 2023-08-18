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
#' @param group
#'
#' group name of a leaflet layer group
#'
#' @param className
#'
#' extra CSS class to append to the control, space separated
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
#' colors <- c('blue', 'red', 'yellow', 'green', 'orange', 'purple')
#' i <- as.integer(cut(quakes$mag, breaks = quantile(quakes$mag, seq(0,1,1/6)),
#'                     include.lowest = TRUE))
#' leafImg <- system.file(sprintf('img/leaf-%s.png', colors),
#'                        package = 'leaflegend')
#' leafIcons <- icons(
#'   iconUrl = leafImg[i],
#'   iconWidth = 133/236 * 50, iconHeight = 50
#' )
#' leaflet(data = quakes) %>% addTiles() %>%
#'   addMarkers(~long, ~lat, icon = leafIcons) %>%
#'   addLegendImage(images = leafImg,
#'                  labels = colors,
#'                  width = 133/236 * 50,
#'                  height = 50,
#'                  orientation = 'vertical',
#'                  title = htmltools::tags$div('Leaf',
#'                                              style = 'font-size: 24px;
#'                                              text-align: center;'),
#'                  position = 'topright')
#'
#'  # use raster images with size encodings
#'  height <- sizeNumeric(quakes$depth, baseSize = 40)
#'  width <- height * 38 / 95
#'  symbols <- icons(
#'    iconUrl = leafImg[4],
#'    iconWidth = width,
#'    iconHeight = height)
#'  probs <- c(.2, .4, .6, .8)
#'  leaflet(quakes) %>%
#'    addTiles() %>%
#'    addMarkers(icon = symbols,
#'               lat = ~lat, lng = ~long) %>%
#'    addLegendImage(
#'      images = rep(leafImg[4], 4),
#'      labels = round(quantile(height, probs = probs), 0),
#'      width = quantile(height, probs = probs) * 38 / 95,
#'      height = quantile(height, probs = probs),
#'      title = htmltools::tags$div(
#'        'Leaf',
#'        style = 'font-size: 24px; text-align: center; margin-bottom: 5px;'),
#'      position = 'topright', orientation = 'vertical')
addLegendImage <- function(
    map,
    images,
    labels,
    title = NULL,
    labelStyle = 'font-size: 24px; vertical-align: middle;',
    orientation = c('vertical', 'horizontal'),
    width = 20,
    height = 20,
    group = NULL,
    className = 'info legend leaflet-control',
    ...) {
  stopifnot(length(images) == length(labels))
  stopifnot( all(width >= 0) && all(height >= 0) )
  orientation <- match.arg(orientation)
  if ( orientation == 'vertical' ) {
    htmlTag <- htmltools::tags$div
  } else {
    htmlTag <- htmltools::tags$span
  }
  if ( inherits(images, 'svgURI') ) {
    images <- list(images)
  }
  htmlElements <- Map(
    img = images,
    label = labels,
    htmlTag = list(htmlTag),
    width = width,
    height = height,
    maxWidth = max(width) * (orientation == 'vertical'),
    f =
      function(img, label, htmlTag, height, width, maxWidth) {
        marginWidth <- max(0, (maxWidth - width) / 2)
        imgStyle <- sprintf(
      'vertical-align: %s; margin: %spx; margin-right: %spx; margin-left: %spx',
      'middle', 5, marginWidth, marginWidth)
        if ( inherits(img, 'svgURI') ) {
          imgTag <- htmltools::tags$img(
            src = img,
            style = imgStyle,
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
            style = imgStyle,
            height = height,
            width = width
          )
        }
        htmlTag(imgTag, htmltools::tags$span(label, style = labelStyle))
      }
  )
  htmlElements <- addTitle(title = title, htmlElements = htmlElements)
  leaflegendAddControl(map, html = htmltools::tagList(htmlElements),
                       className = className, group = group, ...)
}

#' Create Map Symbols for 'leaflet' maps
#'
#'
#'
#'
#' @param shape
#'
#' the desired shape of the symbol, See \link[leaflegend]{availableShapes}
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
#' @param strokeWidth
#'
#' width in pixels of symbol outline
#'
#' @param ...
#'
#' arguments to be passed to svg shape tag
#'
#' @return
#'
#' HTML svg element
#'
#' @name mapSymbols
#'
#' @export
#'
makeSymbol <- function(shape, width, height = width, color, fillColor = color,
                       opacity = 1, fillOpacity = opacity, ...) {
  stopifnot(is.numeric(width) & is.numeric(height))
  stopifnot(is.numeric(opacity) & is.numeric(fillOpacity))
  stopifnot(!is.na(shape))
  if (shape %in% availableShapes()[['default']]) {
    svg <- symbolSvg(shape = shape,  width = width, height = height,
      color = color, fillColor = fillColor, opacity = opacity,
      fillOpacity = fillOpacity, ...)
  } else if (shape %in% availableShapes()[['pch']] || shape %in%
      (seq_along(availableShapes()[['pch']]) - 1)) {
    svg <- pchSvg(shape = shape,  width = width, height = height,
      color = color, fillColor = fillColor, opacity = opacity,
      fillOpacity = fillOpacity, ...)
  } else {
    stop('Argument "shape" is invalid. See `availableShapes()`.')
  }
  strokeWidth <- 1
  if ( 'stroke-width' %in% names(list(...)) ) {
    strokeWidth <- list(...)[['stroke-width']]
  }
  makeSvgUri(svg = svg, width = width, height = height,
    strokeWidth = strokeWidth)
}
symbolSvg <- function(shape, width, height, color, fillColor, opacity,
  fillOpacity, ...) {
  strokeWidth <- 1
  if ( 'stroke-width' %in% names(list(...)) ) {
    strokeWidth <- list(...)[['stroke-width']]
  }
  switch(
    shape,
    'rect' = htmltools::tags$rect(
      id = 'rect',
      x = strokeWidth,
      y = strokeWidth,
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
      cx = height / 2 + strokeWidth,
      cy = height / 2 + strokeWidth,
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
        strokeWidth,
        height + strokeWidth,
        width + strokeWidth,
        height + strokeWidth,
        width / 2  + strokeWidth,
        strokeWidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'plus' = htmltools::tags$polygon(
      id = 'plus',
      points = drawPlus(width = width, height = height, offset = strokeWidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'cross' = htmltools::tags$polygon(
      id = 'cross',
      points = drawCross(width = width, height = height, offset = strokeWidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'diamond' = htmltools::tags$polygon(
      id = 'diamond',
      points = drawDiamond(width = width, height = height,
        offset = strokeWidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'star' = htmltools::tags$polygon(
      id = 'star',
      points = drawStar(width = width, height = height, offset = strokeWidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'stadium' = htmltools::tags$rect(
      id = 'stadium',
      x = strokeWidth,
      y = strokeWidth,
      height = height,
      width = width,
      rx = "25%",
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'line' = htmltools::tags$line(
      id = 'line',
      x1 = 0,
      x2 = width + strokeWidth * 2,
      y1 = height / 2 + strokeWidth,
      y2 = height / 2 + strokeWidth,
      stroke = color,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    'polygon' = htmltools::tags$polygon(
      id = 'polygon',
      points = drawPolygon(n = 5, width = width, height = height,
        offset = strokeWidth),
      stroke = color,
      fill = fillColor,
      'stroke-opacity' = opacity,
      'fill-opacity' = fillOpacity,
      ...
    ),
    stop('Invalid shape argument.')
  )
}
pchSvg <- function(shape, width, height, color, fillColor, opacity,
  fillOpacity, ...) {
  hexPercentOffset <- .8
  strokeWidth <- 1
  if ( 'stroke-width' %in% names(list(...)) ) {
    strokeWidth <- list(...)[['stroke-width']]
  }
  pchShape <-
    list(
      'open-rect' = htmltools::tags$g(
        transform = sprintf('translate(%f %f)', strokeWidth, strokeWidth),
        htmltools::tags$rect(
          id = 'open-rect',
          x = 0,
          y = 0,
          height = height,
          width = width,
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'open-circle' = htmltools::tags$g(
        transform = sprintf('translate(%f %f)', strokeWidth, strokeWidth),
        htmltools::tags$circle(
          id = 'circle',
          cx = height / 2,
          cy = height / 2,
          r = height / 2,
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'open-triangle' = htmltools::tags$g(
        transform = sprintf('translate(%f %f)', strokeWidth, strokeWidth),
        htmltools::tags$polygon(
          id = 'triangle',
          points = drawTriangle(width = width, height = height,
            offset = 0),
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'simple-plus' = htmltools::tags$g(
        transform = sprintf('translate(%f %f)', strokeWidth, strokeWidth),
        htmltools::tags$line(
          id = 'pline1',
          x1 = 0,
          x2 = width,
          y1 = height / 2,
          y2 = height / 2,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'pline2',
          x1 = width / 2,
          x2 = width / 2,
          y1 = 0,
          y2 = height,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'simple-cross' = htmltools::tags$g(
        transform = sprintf('translate(%f %f)', strokeWidth, strokeWidth),
        htmltools::tags$line(
          id = 'cline1',
          x1 = 0,
          x2 = width,
          y1 = 0,
          y2 = height,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'cline2',
          x1 = 0,
          x2 = width,
          y1 = height,
          y2 = 0,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'open-diamond' =  htmltools::tags$polygon(
        id = 'diamond',
        points = drawDiamond(width = width, height = height,
          offset = strokeWidth),
        stroke = color,
        fill = 'transparent',
        'stroke-opacity' = opacity,
        ...
      ),
      'open-down-triangle' = htmltools::tags$polygon(
        id = 'triangle',
        points = drawTriangle(width = width, height = height,
          offset = strokeWidth),
        stroke = color,
        fill = 'transparent',
        'stroke-opacity' = opacity,
        transform = sprintf('rotate(180 %f %f)', height / 2 + strokeWidth,
          width / 2 + strokeWidth),
        ...
      ),
      'cross-rect' = htmltools::tags$g(
        htmltools::tags$line(
          id = 'cline1',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = strokeWidth,
          y2 = height + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'cline2',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = height + strokeWidth,
          y2 = strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$rect(
          id = 'open-rect',
          x = strokeWidth,
          y = strokeWidth,
          height = height,
          width = width,
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'simple-star' = htmltools::tags$g(
        htmltools::tags$line(
          id = 'pline1',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = height / 2 + strokeWidth,
          y2 = height / 2 + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'pline2',
          x1 = width / 2 + strokeWidth,
          x2 = width / 2 + strokeWidth,
          y1 = strokeWidth,
          y2 = height + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'cline1',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = strokeWidth,
          y2 = height + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'cline2',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = height + strokeWidth,
          y2 = strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'plus-diamond' = htmltools::tags$g(
        htmltools::tags$line(
          id = 'pline1',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = height / 2 + strokeWidth,
          y2 = height / 2 + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'pline2',
          x1 = width / 2 + strokeWidth,
          x2 = width / 2 + strokeWidth,
          y1 = strokeWidth,
          y2 = height + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$polygon(
          id = 'diamond',
          points = drawDiamond(width = width, height = height,
            offset = strokeWidth),
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'plus-circle' = htmltools::tags$g(
        htmltools::tags$line(
          id = 'pline1',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = height / 2 + strokeWidth,
          y2 = height / 2 + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'pline2',
          x1 = width / 2 + strokeWidth,
          x2 = width / 2 + strokeWidth,
          y1 = strokeWidth,
          y2 = height + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$circle(
          id = 'circle',
          cx = height / 2 + strokeWidth,
          cy = height / 2 + strokeWidth,
          r = height / 2,
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'hexagram' = htmltools::tags$g(
        transform = sprintf('translate(%f %f)', strokeWidth,
          strokeWidth + height * (1 - hexPercentOffset) / 2),
        htmltools::tags$polygon(
          id = 'triangle',
          points = drawTriangle(width = width * hexPercentOffset,
            height = height * hexPercentOffset,
            offset = 0),
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          transform = sprintf('rotate(180 %f %f) translate(%f %f)',
            height * hexPercentOffset / 2,
            width * hexPercentOffset / 2,
            -width * (1 - hexPercentOffset) / 2,
            -height * (1 - hexPercentOffset) / 2),
          ...
        ),
        htmltools::tags$polygon(
          id = 'triangle',
          points = drawTriangle(width = width * hexPercentOffset,
            height = height * hexPercentOffset,
            offset = 0),
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          transform = sprintf('translate(%f %f)',
            width * (1 - hexPercentOffset) / 2,
            -height * (1 - hexPercentOffset) / 2),
          ...
        )
      ),
      'plus-rect' = htmltools::tags$g(
        htmltools::tags$line(
          id = 'pline1',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = height / 2 + strokeWidth,
          y2 = height / 2 + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'pline2',
          x1 = width / 2 + strokeWidth,
          x2 = width / 2 + strokeWidth,
          y1 = strokeWidth,
          y2 = height + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$rect(
          id = 'open-rect',
          x = strokeWidth,
          y = strokeWidth,
          height = height,
          width = width,
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'cross-circle' = htmltools::tags$g(
        htmltools::tags$line(
          id = 'cline1',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = strokeWidth,
          y2 = height + strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$line(
          id = 'cline2',
          x1 = strokeWidth,
          x2 = width + strokeWidth,
          y1 = height + strokeWidth,
          y2 = strokeWidth,
          stroke = color,
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$circle(
          id = 'circle',
          cx = height / 2 + strokeWidth,
          cy = height / 2 + strokeWidth,
          r = height / 2,
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'triangle-rect' = htmltools::tags$g(
        htmltools::tags$polygon(
          id = 'triangle',
          points = drawTriangle(width = width, height = height,
            offset = strokeWidth),
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        ),
        htmltools::tags$rect(
          id = 'open-rect',
          x = strokeWidth,
          y = strokeWidth,
          height = height,
          width = width,
          stroke = color,
          fill = 'transparent',
          'stroke-opacity' = opacity,
          ...
        )
      ),
      'solid-rect' = htmltools::tags$rect(
        id = 'rect',
        x = strokeWidth,
        y = strokeWidth,
        height = height,
        width = width,
        stroke = 'transparent',
        fill = coalesce_missing(fillColor, color),
        'fill-opacity' = fillOpacity,
        ...
      ),
      'solid-circle-md' = htmltools::tags$circle(
        id = 'circle',
        cx = height / 2 + strokeWidth,
        cy = height / 2 + strokeWidth,
        r =  height *  3 / 4 / 2,
        stroke = 'transparent',
        fill = coalesce_missing(fillColor, color),
        'fill-opacity' = fillOpacity,
        ...
      ),
      'solid-triangle' = htmltools::tags$polygon(
        id = 'triangle',
        points = drawTriangle(width = width, height = height,
          offset = strokeWidth),
        stroke = 'transparent',
        fill = coalesce_missing(fillColor, color),
        'fill-opacity' = fillOpacity,
        ...
      ),
      'solid-diamond' = htmltools::tags$polygon(
        id = 'diamond',
        points = drawDiamond(width = width, height = height,
          offset = strokeWidth),
        stroke = 'transparent',
        fill = coalesce_missing(fillColor, color),
        'fill-opacity' = fillOpacity,
        ...
      ),
      'solid-circle-bg' = htmltools::tags$circle(
        id = 'circle',
        cx = height / 2 + strokeWidth,
        cy = height / 2 + strokeWidth,
        r =  height *  4 / 4 / 2,
        stroke = 'transparent',
        fill = coalesce_missing(fillColor, color),
        'fill-opacity' = fillOpacity,
        ...
      ),
      'solid-circle-sm' = htmltools::tags$circle(
        id = 'circle',
        cx = height / 2 + strokeWidth,
        cy = height / 2 + strokeWidth,
        r = height *  2 / 4 / 2,
        stroke = 'transparent',
        fill = coalesce_missing(fillColor, color),
        'fill-opacity' = fillOpacity,
        ...
      ),
      'circle' = htmltools::tags$circle(
        id = 'circle',
        cx = height / 2 + strokeWidth,
        cy = height / 2 + strokeWidth,
        r = height / 2,
        stroke = color,
        fill = fillColor,
        'stroke-opacity' = opacity,
        'fill-opacity' = fillOpacity,
        ...
      ),
      'rect' = htmltools::tags$rect(
        id = 'rect',
        x = strokeWidth,
        y = strokeWidth,
        height = height,
        width = width,
        stroke = color,
        fill = fillColor,
        'stroke-opacity' = opacity,
        'fill-opacity' = fillOpacity,
        ...
      ),
      'diamond' = htmltools::tags$polygon(
        id = 'diamond',
        points = drawDiamond(width = width, height = height,
          offset = strokeWidth),
        stroke = color,
        fill = fillColor,
        'stroke-opacity' = opacity,
        'fill-opacity' = fillOpacity,
        ...
      ),
      'triangle' = htmltools::tags$polygon(
        id = 'triangle',
        points = drawTriangle(width = width, height = height,
          offset = strokeWidth),
        stroke = color,
        fill = fillColor,
        'stroke-opacity' = opacity,
        'fill-opacity' = fillOpacity,
        ...
      ),
      'down-triangle' = htmltools::tags$polygon(
        id = 'triangle',
        points = drawTriangle(width = width, height = height,
          offset = strokeWidth),
        stroke = color,
        fill = fillColor,
        'stroke-opacity' = opacity,
        'fill-opacity' = fillOpacity,
        transform = sprintf('rotate(180 %f %f)', height / 2 + strokeWidth,
          width / 2 + strokeWidth),
        ...
      )
    )
  if (is.numeric(shape)) {
    shape <- shape + 1L
  } else {
    if (!shape %in% names(pchShape)) {
      stop(sprintf('"%s" is not a valid pch name', shape))
    }
  }
  pchShape[[shape]]
}
coalesce_missing <- function(x, y) {
  if (missing(x)) y else x
}
#' @param svg
#'
#' inner svg tags for symbol
#'
#' @name mapSymbols
#'
#' @export
#'
makeSvgUri <- function(svg, width, height, strokeWidth) {
  svgURI <-
    sprintf('data:image/svg+xml,%s',
      utils::URLencode(as.character(
        htmltools::tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          version = "1.1",
          width = width + strokeWidth * 2,
          height = height + strokeWidth * 2,
          svg
        )
      ), reserved = TRUE))
  structure(svgURI, class = c(class(svgURI), 'svgURI'))
}

makeLegendSymbol <- function(label, labelStyle,
  imgStyle = "vertical-align: middle; margin: 1px;", ...) {
  shapeTag <- makeSymbol(...)
  htmltools::tagList(
    htmltools::tags$img(src = shapeTag,
                        style = imgStyle),
    htmltools::tags$span(label,
                         style =
                           sprintf("vertical-align: middle; padding: 1px; %s",
                                         labelStyle))
  )
}

drawPlus <- function(width, height, offset = 0) {
  x <- width * c(rep(c(.4, 0, .4, .6, 1, .6), each = 2), .4) + offset
  y <- height * c(0, rep(c(.4, .6, 1, .6, .4, 0), each = 2)) + offset
  paste(x, y, sep = ',', collapse = ' ')
}

drawDiamond <- function(width, height, offset = 0) {
  x <- width * c( .5, 0, .5, 1, .5) + offset
  y <- height * c(0, .5, 1, .5, 0) + offset
  paste(x, y, sep = ',', collapse = ' ')
}

drawCross <- function(width, height, offset = 0) {
  a <- sqrt(2) / 10
  x <- width *  c(a, 0, .5 - a, 0, a, .5, 1 - a, 1, .5 + a, 1, 1 - a, .5, a) +
    offset
  y <- height * c(0, a, .5, 1 - a, 1, .5 + a, 1, 1 - a, .5, a, 0, .5 - a, 0) +
    offset
  paste(x, y, sep = ',', collapse = ' ')
}
drawStar <- function(width, height, offset = 0) {
  x <- width * c(0.4, 0.4, 0.1414214, 0, 0.2585786, 0, 0, 0.2585786, 0,
                 0.1414214, 0.4, 0.4, 0.6, 0.6, 0.8585786, 1, 0.7414214, 1, 1,
                 0.7414214, 1, 0.8585786, 0.6, 0.6, 0.4) + offset
  y <- height * c(0, 0.2585786, 0, 0.1414214, 0.4, 0.4, 0.6, 0.6, 0.8585786, 1,
                  0.7414214, 1, 1, 0.7414214, 1, 0.8585786, 0.6, 0.6, 0.4, 0.4,
                  0.1414214, 0, 0.2585786, 0, 0) + offset
  paste(x, y, sep = ',', collapse = ' ')
}
drawPolygon <- function(n, width = 1, height = 1, offset = 0) {
  stopifnot(n > 0 || !is.integer(n))
  radians <- seq(-pi, pi, by = 2 * pi / n)
  if ( n %% 2 == 0 ) {
    x <- (cos(radians) + 1) * 1 / 2 * width + offset
    y <- (sin(radians) + 1) * 1 / 2 * height + offset
  } else {
    radians <- seq(-pi, pi, by = 2 * pi / n)
    x <- (sin(radians) + 1) * 1 / 2 * width + offset
    y <- (cos(radians) + 1) * 1 / 2 * height + offset
  }
  paste(x, y, sep = ',', collapse = ' ')
}
drawTriangle <- function(width, height, offset) {
  sprintf('%s,%s %s,%s %s,%s',
    offset,
    height + offset,
    width + offset,
    height + offset,
    width / 2  + offset,
    offset)
}

#' @export
#'
#' @rdname mapSymbols
makeSymbolIcons <- function(shape,
  color,
  fillColor = color,
  opacity,
  fillOpacity = opacity,
  strokeWidth = 1,
  width,
  height = width,
  ...) {
  symbols <- Map(
    makeSymbol,
    shape = shape,
    width = width,
    height = height,
    color = color,
    fillColor = fillColor,
    opacity = opacity,
    fillOpacity = fillOpacity,
    `stroke-width` = strokeWidth,
    ...
  )
  leaflet::icons(
    iconUrl = unname(symbols),
    iconAnchorX = width / 2,
    iconAnchorY = height / 2
  )
}
#' @param map
#'
#' a map widget object created from 'leaflet'
#'
#' @param lng
#'
#' a numeric vector of longitudes, or a one-sided formula of the form \code{~x}
#' where \code{x} is a variable in \code{data}; by default
#' (if not explicitly provided), it will be automatically inferred from data
#' by looking for a column named \code{lng}, \code{long}, or \code{longitude}
#' (case-insensitively)
#'
#' @param lat
#'
#' a vector of latitudes or a formula (similar to the \code{lng} argument; the
#' names \code{lat} and \code{latitude} are used when guessing the latitude
#' column from \code{data})
#'
#' @param values
#'
#' the values used to generate shapes; can be omitted for a single type of
#' shape
#'
#' @param shape
#'
#' the desired shape of the symbol, See \link[leaflegend]{availableShapes}
#'
#' @param color
#'
#' stroke color
#'
#' @param fillColor
#'
#' fill color
#'
#' @param opacity
#'
#' stroke opacity
#'
#' @param fillOpacity
#'
#' fill opacity
#'
#' @param strokeWidth
#'
#' stroke width in pixels
#'
#' @param width
#'
#' in pixels
#'
#' @param height
#'
#' in pixels
#'
#' @param data
#'
#' the data object from which the argument values are derived; by default, it
#' is the \code{data} object provided to \code{leaflet()} initially, but can be
#' overridden
#'
#' @param ...
#'
#' arguments to be passed to \link[leaflet]{addMarkers}
#'
#' @export
#'
#' @rdname mapSymbols
addSymbols <- function(
    map,
    lng,
    lat,
    values,
    shape,
    color,
    fillColor = color,
    opacity = 1,
    fillOpacity = opacity,
    strokeWidth = 1,
    width = 20,
    height = width,
    data = leaflet::getMapData(map),
    ...
) {
  if (missing(shape)) {
    shape <- availableShapes()[['default']]
  }
  if ( !missing(values) ) {
    values <- as.factor(parseValues(values, data))
    if ( length(levels(values)) > length(shape) ) {
      stop('values has more factor levels than shape. Maximum levels is 7')
    }
    shape <- shape[values]
  } else {
    shape <- shape[1]
  }
  if ( inherits(color, 'formula') ) {
    color <- parseValues(color, data)
  }
  if ( inherits(fillColor, 'formula') ) {
    fillColor <- parseValues(fillColor, data)
  }
  iconSymbols <- makeSymbolIcons(shape = shape, color = color,
                                 fillColor = fillColor, opacity = opacity,
                                 fillOpacity = fillOpacity,
                                 strokeWidth = strokeWidth, width = width,
                                 height = width)
  leaflet::addMarkers(map = map, lng = lng, lat = lat, icon = iconSymbols,
                      data = data, ...)
}
#' @export
#'
#' @rdname mapSymbols
addSymbolsSize <- function(
    map,
    lng,
    lat,
    values,
    shape,
    color,
    fillColor = color,
    opacity = 1,
    fillOpacity = opacity,
    strokeWidth = 1,
    baseSize = 20,
    data = leaflet::getMapData(map),
    ...
) {
  values <- parseValues(values, data)
  sizes <- sizeNumeric(values, baseSize)
  if ( inherits(color, 'formula') ) {
    color <- parseValues(color, data)
  }
  if ( inherits(fillColor, 'formula') ) {
    fillColor <- parseValues(fillColor, data)
  }
  addSymbols(map = map, lng = lng, lat = lat, shape = shape, color = color,
             fillColor = fillColor, opacity = opacity,
             fillOpacity = fillOpacity, strokeWidth = strokeWidth,
             width = sizes, data = data, ...)
}

#' Add Customizable Color Legends to a 'leaflet' map widget
#'
#' Functions for more control over the styling of 'leaflet' legends.
#' The 'leaflet'
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
#' an approximate number of tick-marks on the color gradient for the
#' colorNumeric palette
#'
#' @param title
#'
#' the legend title, pass in HTML to style
#'
#' @param shape
#'
#' the desired shape of the symbol, See \link[leaflegend]{availableShapes}
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
#' @param group
#'
#' group name of a leaflet layer group
#'
#' @param labels
#'
#' labels
#'
#' @param naLabel
#'
#' the legend label for NAs in values
#'
#' @param className
#'
#' extra CSS class to append to the control, space separated
#'
#' @param data a data object. Currently supported objects are matrices, data
#'   frames, spatial objects from the \pkg{sp} package
#'   (\code{SpatialPoints}, \code{SpatialPointsDataFrame}, \code{Polygon},
#'   \code{Polygons}, \code{SpatialPolygons}, \code{SpatialPolygonsDataFrame},
#'   \code{Line}, \code{Lines}, \code{SpatialLines}, and
#'   \code{SpatialLinesDataFrame}), and
#'   spatial data frames from the \pkg{sf} package.
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
#'     title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px;
#'     color: red;'),
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
# binPal <- colorBin('Set1', quakes$mag)
# leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(
#     data = quakes,
#     lat = ~ lat,
#     lng = ~ long,
#     color = ~ binPal(mag),
#     opacity = 1,
#     fillOpacity = 1
#   ) %>%
#   addLegendBin(
#     pal = binPal,
#     position = 'topright',
#     values = ~mag,
#     title = 'addLegendBin',
#     labelStyle = 'font-size: 18px; font-weight: bold;',
#     orientation = 'horizontal'
#   ) %>%
#   addLegend(pal = binPal,
#             values = quakes$mag,
#             title = 'addLegend')
#'
#' # Group Layer Control
#' # Works with baseGroups and overlayGroups
#'
# leaflet() %>%
#   addTiles() %>%
#   addLegendNumeric(
#     pal = numPal,
#     values = quakes$depth,
#     position = 'topright',
#     title = 'addLegendNumeric',
#     group = 'Numeric Data'
#   ) %>%
#   addLegendQuantile(
#     pal = quantPal,
#     values = quakes$mag,
#     position = 'topright',
#     title = 'addLegendQuantile',
#     group = 'Quantile'
#   ) %>%
#   addLegendBin(
#     pal = binPal,
#     position = 'bottomleft',
#     title = 'addLegendBin',
#     group = 'Bin',
#     values = ~mag
#   ) %>%
#   addLayersControl(
#     baseGroups = c('Numeric Data', 'Quantile'),  overlayGroups = c('Bin'),
#     position = 'bottomright'
#   )
addLegendNumeric <- function(map,
                             pal,
                             values,
                             title = NULL,
                             #labelStyle = 'font-size: 24px;',
                             shape = c('rect', 'stadium'),
                             orientation = c('vertical', 'horizontal'),
                             width = 20,
                             height = 100,
                             bins = 7,
                             numberFormat = function(x) {
                               prettyNum(x, format = 'f', big.mark = ',',
                                         digits = 3, scientific = FALSE)
                               },
                             tickLength = 4,
                             tickWidth = 1,
                             decreasing = FALSE,
                             fillOpacity = 1,
                             group = NULL,
                             labels = NULL,
                             naLabel = 'NA',
                             className = 'info legend leaflet-control',
                             data = leaflet::getMapData(map),
                             ...) {
  stopifnot(is.logical(decreasing))
  stopifnot(attr(pal, 'colorType') == 'numeric')
  stopifnot(is.numeric(width) && is.numeric(height) && width >= 0 &&
               height >= 0)
  stopifnot(is.numeric(tickLength) && is.numeric(tickWidth) &&
               tickLength >= 0 && tickWidth >= 0)
  shape <- match.arg(shape)
  id <- sprintf('gradient-%s-%d',
                gsub('[[:punct:]]|\\s', '', deparse(match.call()[['values']])),
                length(map[["x"]][["calls"]]) + 1)
  values <- parseValues(values = values, data = data)
  rng <- range(values, na.rm = TRUE)
  breaks <- pretty(values, bins)
  orientation <- match.arg(orientation)
  vertical <- orientation == 'vertical'
  if (breaks[1] < rng[1]) {
    breaks[1] <- rng[1]
  }
  if (breaks[length(breaks)] > rng[2]) {
    breaks[length(breaks)] <- rng[2]
  }
  colors <- pal(breaks)
  hasNa <- any(is.na(values))
  if (vertical) {
    htmlElements <- makeNumericVertical(id = id, breaks = breaks,
      labels = labels, colors = colors, decreasing = decreasing,
      hasNa = hasNa, tickLength = tickLength, tickWidth = tickWidth,
      rng = rng, height = height, width = width, fillOpacity = fillOpacity,
      shape = shape, naColor = pal(NA), naLabel = naLabel, title = title,
      numberFormat = numberFormat)
  } else {
    htmlElements <- makeNumericHorizontal(id = id, breaks = breaks,
      labels = labels, colors = colors, decreasing = decreasing,
      hasNa = hasNa, tickLength = tickLength, tickWidth = tickWidth,
      rng = rng, height = height, width = width, fillOpacity = fillOpacity,
      shape = shape, naColor = pal(NA), naLabel = naLabel, title = title,
      numberFormat = numberFormat)
  }
  leaflegendAddControl(map, html = htmlElements, className = className,
    group = group, ...)
}


makeNumericHorizontal <- function(id, breaks, labels, colors, decreasing, hasNa,
  tickWidth, tickLength, rng, height, width, fillOpacity, shape, naColor,
  naLabel, title, numberFormat) {
  x1 <- 0
  x2 <- 1
  y1 <- 0
  y2 <- 0
  outer <- c(1L, length(breaks))
  if (isTRUE(decreasing)) {
    x1 <- 1
    x2 <- 0
    labels <- rev(labels)
  }
  scaledbreaks <- (breaks - rng[1]) / (rng[2] - rng[1])
  offsets <- sprintf('%.3f%%', scaledbreaks * 100)
  breaks <- breaks[outer]
  if (is.null(labels)) {
    labels <- numberFormat(breaks)
  }
  colors <- colors
  scaledbreaks <- scaledbreaks[outer]
  svgwidth <- width
  svgheight <- height + tickLength
  rectx <- 0
  linex1 <- scaledbreaks * width
  linex2 <- scaledbreaks * width
  liney1 <- height
  liney2 <- height + tickLength
  naSize <- height
  labelStyle <- ''
  ry <- '0%'
  if ( shape == 'stadium' ) {
    ry <- '10%'
  }
  rectround <- list(ry = ry)
  svgElement <- htmltools::tags$svg(
    width = svgwidth,
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
    )
  )
  cexAdj <- 1.22
  pixel2Inch <- 72
  textWidth <- graphics::strwidth(labels, units = 'inches', cex = cexAdj) *
    pixel2Inch
  maxTextWidth <- max(textWidth)
  left1 <- 0
  if (textWidth[1] < maxTextWidth  ) {
    left1 <- (maxTextWidth / 2 - textWidth[1] / 2) / (width + maxTextWidth)
  }
  left2 <- (width) / (width + maxTextWidth)
  if (textWidth[2] < maxTextWidth) {
    left2 <- (width + maxTextWidth / 2 - textWidth[2] / 2) / (width + maxTextWidth)
  }
  maxTextWidth <- max(textWidth)
  htmlElements <- list(
    htmltools::tags$div(
      style = sprintf('margin-right: %spx; margin-left: %spx',
        maxTextWidth / 2, maxTextWidth / 2 ), svgElement),
    htmltools::tags$div(
      style = sprintf("width: %.3f; height: 1rem; position: relative; %s",
        width + maxTextWidth, labelStyle),
      htmltools::tags$div(
        style = sprintf("position:absolute; left:%.3f%%; top: 0%%;",
          left1 * 100),
        labels[1]),
      htmltools::tags$div(
        style = sprintf("position:absolute; left:%.3f%%; top: 0%%;",
          left2 * 100
          ),
        labels[2])

    )
  )
  htmlElements <- addTitle(title = title, htmlElements = htmlElements)
  htmlElements <- addNa(hasNa = hasNa, htmlElements = htmlElements,
    shape = shape, labels = naLabel, colors = naColor, labelStyle = labelStyle,
    height = naSize, width = naSize, opacity = fillOpacity,
    fillOpacity = fillOpacity, strokeWidth = 0)
  htmltools::tagList(htmlElements)
}

makeNumericVertical <- function(id, breaks, labels, colors, decreasing, hasNa,
  tickWidth, tickLength, rng, height, width, fillOpacity, shape, naColor,
  naLabel, title, numberFormat) {
  x1 <- 0
  x2 <- 0
  y1 <- 0
  y2 <- 1
  outer <- c(1, length(breaks))
  if (is.null(labels)) {
    labels <- numberFormat(breaks)[-outer]
  }
  if (isTRUE(decreasing)) {
    y1 <- 1
    y2 <- 0
    labels <- rev(labels)
  }
  scaledbreaks <- (breaks - rng[1]) / (rng[2] - rng[1])
  svgwidth <- width + tickLength
  svgheight <- height
  rectx <- 0
  linex1 <- width
  linex2 <- width + tickLength
  liney1 <- scaledbreaks[-outer] * height
  liney2 <- scaledbreaks[-outer] * height
  naSize <- width
  labelStyle <- ''
  rx <- '0%'
  if (shape == 'stadium') {
    rx <- '10%'
  }
  rectround <- list(rx = rx)
  svgElement <- htmltools::tags$svg(
    width = svgwidth,
    height = svgheight,
    style = 'margin: 1px;',
    htmltools::tags$def(
      htmltools::tags$linearGradient(
        id = id,
        x1 = x1, y1 = y1, x2 = x2, y2 = y2,
        htmltools::tagList(Map(htmltools::tags$stop,
          offset = sprintf('%.3f%%', scaledbreaks * 100),
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
    )
  )
  cexAdj <- 1.22
  pixel2Inch <- 72
  textWidth <- max(graphics::strwidth(labels, units = 'inches',
    cex = cexAdj)) * pixel2Inch
  textHeight <- max(graphics::strheight(labels, units = 'inches',
    cex = 1)) * pixel2Inch
  htmlElements <- list(htmltools::tags$div(style = 'display: flex;',
    htmltools::tags$div(svgElement, style = "margin-right: 5px"),
    htmltools::tags$div(
      style = sprintf("width: %.3fpx; height: %.3fpx; display: flex;
        justify-content: flex-end; position: relative; %s",
        textWidth, height, labelStyle),
      class = "container",
      Map(function(y, label) {
        htmltools::tags$div(
          style = sprintf("position:absolute; top: %.3f%%;", y),
          htmltools::HTML(label))
      },
        y = (scaledbreaks[-outer] - textHeight / height) * 100,
        label = labels
      )
    )
    , htmltools::tags$div(style = "width: 8px; position: relative;")
  ))
  htmlElements <- addTitle(title, htmlElements)
  htmlElements <- addNa(hasNa = hasNa, htmlElements = htmlElements,
    shape = shape, labels = naLabel, colors = naColor, labelStyle = labelStyle,
    height = naSize, width = naSize, opacity = fillOpacity,
    fillOpacity = fillOpacity, strokeWidth = 0)
  htmltools::tagList(htmlElements)
}

addTitle <- function(title, htmlElements) {
  if (is.null(title)) {
    NULL
  } else if (inherits(title, 'shiny.tag')) {
    title <- list(htmltools::div(title))
  } else if (is.character(title)) {
    title <- list(htmltools::div(htmltools::tags$strong(title)))
  } else {
    stop('Title must be character vector or an html tags object')
  }
  append(htmlElements, title, after = 0)
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
                              shape = 'rect',
                              orientation = c('vertical', 'horizontal'),
                              width = 24,
                              height = 24,
                              numberFormat = function(x) {
                                prettyNum(x, big.mark = ',', scientific = FALSE,
                                          digits = 1)
                                },
                              opacity = 1,
                              fillOpacity = opacity,
                              group = NULL,
                              className = 'info legend leaflet-control',
                              naLabel = 'NA',
                              data = leaflet::getMapData(map),
                              ...) {
  stopifnot( attr(pal, 'colorType') == 'quantile' )
  stopifnot( width >= 0 && height >= 0 )
  orientation <- match.arg(orientation)
  probs <- attr(pal, 'colorArgs')[['probs']]
  values <- parseValues(values = values, data = data)
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
  htmlElements <- makeLegendCategorical(shape = shape, labels = labels,
    colors = colors,
    labelStyle = labelStyle,
    height = height, width = width,
    opacity = opacity,
    fillOpacity = fillOpacity,
    orientation = orientation,
    title = title,
    hasNa = any(is.na(values)),
    naLabel = naLabel,
    naColor = pal(NA))
  leaflegendAddControl(map, html = htmltools::tagList(htmlElements),
                       className = className, group = group, ...)
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
                         shape = 'rect',
                         orientation = c('vertical', 'horizontal'),
                         width = 24,
                         height = 24,
                         numberFormat = function(x) {
                           format(round(x, 3), big.mark = ',', trim = TRUE,
                                  scientific = FALSE)
                         },
                         opacity = 1,
                         fillOpacity = opacity,
                         group = NULL,
                         className = 'info legend leaflet-control',
                         naLabel = 'NA',
                         data = leaflet::getMapData(map),
                         ...) {
  stopifnot( attr(pal, 'colorType') == 'bin' )
  stopifnot( width >= 0 && height >= 0 )
  orientation <- match.arg(orientation)
  values <- parseValues(values = values, data = data)
  bins <- attr(pal, 'colorArgs')[['bins']]
  labels <- sprintf(' %s - %s', numberFormat(bins[-length(bins)]),
                    numberFormat(bins[-1]))
  colors <- pal((bins[-1] + bins[-length(bins)]) / 2 )
  htmlElements <- makeLegendCategorical(shape = shape, labels = labels,
    colors = colors,
    labelStyle = labelStyle,
    height = height, width = width,
    opacity = opacity,
    fillOpacity = fillOpacity,
    orientation = orientation,
    title = title,
    hasNa = any(is.na(values)),
    naLabel = naLabel,
    naColor = pal(NA))
  leaflegendAddControl(map, html = htmltools::tagList(htmlElements),
                       className = className, group = group, ...)
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
                            shape = 'rect',
                            orientation = c('vertical', 'horizontal'),
                            width = 24,
                            height = 24,
                            opacity = 1,
                            fillOpacity = opacity,
                            group = NULL,
                            className = 'info legend leaflet-control',
                            naLabel = 'NA',
                            data = leaflet::getMapData(map),
                            ...) {
  stopifnot( attr(pal, 'colorType') == 'factor' )
  stopifnot( width >= 0 && height >= 0 )
  orientation <- match.arg(orientation)
  values <- parseValues(values = values, data = data)
  hasNa <- any(is.na(values))
  values <- sort(unique(values))
  labels <- sprintf(' %s', values)
  colors <- pal(values)
  htmlElements <- makeLegendCategorical(shape = shape, labels = labels,
                                        colors = colors,
                                        labelStyle = labelStyle,
                                        height = height, width = width,
                                        opacity = opacity,
                                        fillOpacity = fillOpacity,
                                        orientation = orientation,
                                        title = title,
                                        hasNa = hasNa,
                                        naLabel = naLabel,
                                        naColor = pal(NA))
  leaflegendAddControl(map, html = htmltools::tagList(htmlElements),
                       className = className, group = group, ...)
}

makeLegendCategorical <- function(shape, labels, colors, labelStyle, height,
                              width, opacity, fillOpacity, orientation, title,
  hasNa, naLabel, naColor) {
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
  if ( orientation == 'vertical' ) {
    htmlElements <- lapply(htmlElements, htmltools::tagList,
                           htmltools::tags$br())
  }
  htmlElements <- addTitle(title = title, htmlElements = htmlElements)
  htmlElements <- addNa(hasNa = hasNa, htmlElements = htmlElements,
    shape = shape, labels = naLabel, colors = naColor, labelStyle = labelStyle,
    height = height, width = width, opacity = fillOpacity,
    fillOpacity = fillOpacity, strokeWidth = 1)
  htmlElements
}

addNa <- function(hasNa, htmlElements, shape, labels, colors,
  labelStyle, height,  width, opacity, fillOpacity, strokeWidth) {
  if (hasNa) {
    naLegend <- list(htmltools::div(
      style = 'margin-top: .3rem;',
      makeLegendSymbol(
        shape = shape,
        label = labels,
        color = colors,
        labelStyle = labelStyle,
        height = height,
        width = width,
        opacity = opacity,
        fillOpacity = fillOpacity,
        'stroke-width' = strokeWidth,
        imgStyle = 'vertical-align: middle; margin: 1px;'
      )))
    htmlElements <- append(htmlElements, naLegend)
  }
  htmlElements
}

#' Add a legend for the sizing of symbols or the width of lines
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
#' the values used to generate sizes and if colorValues is not specified and
#' pal is given, then the values are used to generate  colors from the palette
#' function
#'
#' @param title
#'
#' the legend title, pass in HTML to style
#'
#' @param shape
#'
#' the desired shape of the symbol, See \link[leaflegend]{availableShapes}
#'
#' @param orientation
#'
#' stack the legend items vertically or horizontally
#'
#'
#' @param labelStyle
#'
#' character string of style argument for HTML text
#'
#'
#' @param opacity
#'
#' opacity of the legend items
#'
#' @param fillOpacity
#'
#' fill opacity of the legend items
#'
#' @param breaks
#'
#' an integer specifying the number of breaks or a numeric vector of the breaks
#'
#' @param baseSize
#'
#' re-scaling size in pixels of the mean of the values, the average value will
#' be this exact size
#'
#' @param color
#'
#' the color of the legend symbols, if omitted pal is used
#'
#' @param fillColor
#'
#' fill color of symbol
#'
#' @param strokeWidth
#'
#' width of symbol outline
#'
#' @param numberFormat
#'
#' formatting functions for numbers that are displayed e.g. format, prettyNum
#'
#' @param group
#'
#' group name of a leaflet layer group
#'
#' @param className
#'
#' extra CSS class to append to the control, space separated
#'
#' @param data a data object. Currently supported objects are matrices, data
#'   frames, spatial objects from the \pkg{sp} package
#'   (\code{SpatialPoints}, \code{SpatialPointsDataFrame}, \code{Polygon},
#'   \code{Polygons}, \code{SpatialPolygons}, \code{SpatialPolygonsDataFrame},
#'   \code{Line}, \code{Lines}, \code{SpatialLines}, and
#'   \code{SpatialLinesDataFrame}), and
#'   spatial data frames from the \pkg{sf} package.
#'
#' @param ...
#'
#' arguments to pass to
#'
#' \link[leaflet]{addControl} for addLegendSize
#'
#' \link[base]{pretty} for sizeBreaks
#'
#' \link[leaflegend]{makeSymbol} for makeSymbolsSize
#'
#' @return
#'
#' an object from \link[leaflet]{addControl}
#'
#' @export
#'
#' @name legendSymbols
#'
#' @examples
#' library(leaflet)
#' data("quakes")
#' quakes <- quakes[1:100,]
#' numPal <- colorNumeric('viridis', quakes$depth)
#' sizes <- sizeNumeric(quakes$depth, baseSize = 10)
#' symbols <- Map(
#'   makeSymbol,
#'   shape = 'triangle',
#'   color = numPal(quakes$depth),
#'   width = sizes,
#'   height = sizes
#' )
#' leaflet() %>%
#'   addTiles() %>%
#'   addMarkers(data = quakes,
#'              icon = icons(iconUrl = symbols),
#'              lat = ~lat, lng = ~long) %>%
#'   addLegendSize(
#'     values = quakes$depth,
#'     pal = numPal,
#'     title = 'Depth',
#'     labelStyle = 'margin: auto;',
#'     shape = c('triangle'),
#'     orientation = c('vertical', 'horizontal'),
#'     opacity = .7,
#'     breaks = 5)
#'
#' # a wrapper for making icons is provided
#' sizeSymbols <-
#' makeSymbolsSize(
#'   quakes$depth,
#'   shape = 'cross',
#'   fillColor = numPal(quakes$depth),
#'   color = 'black',
#'   strokeWidth = 1,
#'   opacity = .8,
#'   fillOpacity = .5,
#'   baseSize = 20
#' )
#' leaflet() %>%
#'   addTiles() %>%
#'   addMarkers(data = quakes,
#'              icon = sizeSymbols,
#'              lat = ~lat, lng = ~long) %>%
#'   addLegendSize(
#'     values = quakes$depth,
#'     pal = numPal,
#'     title = 'Depth',
#'     shape = 'cross',
#'     orientation = 'horizontal',
#'     strokeWidth = 1,
#'     opacity = .8,
#'     fillOpacity = .5,
#'     color = 'black',
#'     baseSize = 20,
#'     breaks = 5)
#'
#' # Group layers control
#' leaflet() %>%
#'   addTiles() %>%
#'     addLegendSize(
#'       values = quakes$depth,
#'       pal = numPal,
#'       title = 'Depth',
#'       labelStyle = 'margin: auto;',
#'       shape = c('triangle'),
#'       orientation = c('vertical', 'horizontal'),
#'       opacity = .7,
#'       breaks = 5,
#'       group = 'Depth') %>%
#'     addLayersControl(overlayGroups = c('Depth'))
#'
#' # Polyline Legend for Size
#' baseSize <- 10
#' lineColor <- '#00000080'
#' pal <- colorNumeric('Reds', atlStorms2005$MinPress)
#' leaflet() %>%
#'   addTiles() %>%
#'   addPolylines(data = atlStorms2005,
#'                weight = ~sizeNumeric(values = MaxWind, baseSize = baseSize),
#'                color = ~pal(MinPress),
#'                popup = ~as.character(MaxWind)) %>%
#'   addLegendLine(values = atlStorms2005$MaxWind,
#'                 title = 'MaxWind',
#'                 baseSize = baseSize,
#'                 width = 50,
#'                 color = lineColor) %>%
#'   addLegendNumeric(pal = pal,
#'                    title = 'MinPress',
#'                    values = atlStorms2005$MinPress)
addLegendSize <- function(map,
                          pal,
                          values,
                          title = NULL,
                          labelStyle = '',
                          shape = 'rect',
                          orientation = c('vertical', 'horizontal'),
                          color,
                          fillColor = color,
                          strokeWidth = 1,
                          opacity = 1,
                          fillOpacity = opacity,
                          breaks = 5,
                          baseSize = 20,
                          numberFormat = function(x) {
                            prettyNum(x, big.mark = ',', scientific = FALSE,
                                      digits = 1)
                            },
                          group = NULL,
                          className = 'info legend leaflet-control',
                          data = leaflet::getMapData(map),
                          ...) {
  values <- parseValues(values = values, data = data)
  sizes <- sizeBreaks(values, breaks, baseSize)
  if ( missing(color) ) {
    stopifnot( missing(color) & !missing(pal))
    colors <- pal(as.numeric(names(sizes)))
  } else {
    stopifnot(length(color) == 1 || length(color) == length(breaks))
    colors <- color
  }
  if ( missing(fillColor) ) {
    if ( !missing(pal) ) {
      fillColors <- pal(as.numeric(names(sizes)))
    } else {
      fillColors <- colors
    }
  } else {
    stopifnot(length(fillColor) == 1 || length(fillColor) == length(breaks))
    fillColors <- fillColor
  }
  labels <- numberFormat(as.numeric(names(sizes)))
  if (length(names(breaks)) == length(breaks) && length(breaks) > 1) {
    labels <- names(breaks)
  }
  symbols <- Map(makeSymbol,
                 shape = shape,
                 width = sizes,
                 height = sizes,
                 color = colors,
                 fillColor = fillColors,
                 opacity = opacity,
                 fillOpacity = fillOpacity,
                 `stroke-width` = strokeWidth)
  addLegendImage(map, images = symbols,
                 labels = labels,
                 title = title, labelStyle = labelStyle,
                 orientation = orientation, width = sizes, height = sizes,
                 group = group, className = className, ...)

}

#' @export
#'
#' @rdname mapSymbols
sizeNumeric <- function(values, baseSize) {
  stopifnot(baseSize > 0)
  values / mean(values, na.rm = TRUE) * baseSize
}

#' @param breaks
#'
#' an integer specifying the number of breaks or a numeric vector of the breaks;
#' if a named vector then the names are used as labels.
#'
#' @param baseSize
#'
#' re-scaling size in pixels of the mean of the values, the average value will
#' be this exact size
#'
#' @param ...
#'
#' arguments to pass to \code{pretty}
#'
#' @export
#'
#' @rdname mapSymbols
sizeBreaks <- function(values, breaks, baseSize, ...) {
  stopifnot(baseSize > 0)
  if ( length(breaks) == 1 ) {
    breaks <- pretty(values, breaks, ...)
  }
  sizes <- breaks / mean(values, na.rm = TRUE) * baseSize
  stats::setNames(sizes, breaks)[breaks > 0 & breaks <= max(values)]
}

#' @export
#'
#' @rdname mapSymbols
makeSymbolsSize <- function(values,
                          shape = 'rect',
                          color,
                          fillColor,
                          opacity = 1,
                          fillOpacity = opacity,
                          strokeWidth = 1,
                          baseSize,
                          ...
                          ) {
  stopifnot(strokeWidth >= 0)
  stopifnot(length(color) == 1 || length(color) == length(values))
  stopifnot(length(fillColor) == 1 || length(fillColor) == length(values))
  stopifnot(length(shape) < 2)
  sizes <- sizeNumeric(values, baseSize)
  makeSymbolIcons(
    shape = shape,
    width = sizes,
    height = sizes,
    color = color,
    fillColor = fillColor,
    opacity = opacity,
    fillOpacity = fillOpacity,
    strokeWidth = strokeWidth,
    ...
  )
}
#' @param width
#'
#' width in pixels of the lines
#'
#' @export
#'
#' @rdname legendSymbols
addLegendLine <- function(map,
                          pal,
                          values,
                          title = NULL,
                          labelStyle = '',
                          orientation = c('vertical', 'horizontal'),
                          width = 20,
                          color,
                          opacity = 1,
                          fillOpacity = opacity,
                          breaks = 5,
                          baseSize = 10,
                          numberFormat = function(x) {
                            prettyNum(x, big.mark = ',', scientific = FALSE,
                                      digits = 1)
                            },
                          group = NULL,
                          className = 'info legend leaflet-control',
                          data = leaflet::getMapData(map),
                          ...) {
  shape <- 'rect'
  values <- parseValues(values = values, data = data)
  sizes <- sizeBreaks(values, breaks, baseSize)
  if ( missing(color) ) {
    stopifnot( missing(color) & !missing(pal))
    colors <- pal(as.numeric(names(sizes)))
  } else {
    stopifnot(length(color) == 1 || length(color) == length(breaks))
    colors <- color
  }
  labels <- numberFormat(as.numeric(names(sizes)))
  if (length(names(breaks)) == length(breaks) && length(breaks) > 1) {
    labels <- names(breaks)
  }
  symbols <- Map(makeSymbol,
                 shape = shape,
                 width = width,
                 height = sizes,
                 color = 'transparent',
                 fillColor = colors,
                 opacity = opacity,
                 fillOpacity = fillOpacity,
                 `stroke-width` = 0)
  addLegendImage(map, images = symbols,
                 labels = labels,
                 title = title, labelStyle = labelStyle,
                 orientation = orientation, width = width, height = sizes,
                 group = group, className = className, ...)

}

#' @param height
#'
#' in pixels
#'
#' @export
#'
#' @rdname legendSymbols
addLegendSymbol <- function(map,
                            pal,
                            values,
                            title = NULL,
                            labelStyle = '',
                            shape,
                            orientation = c('vertical', 'horizontal'),
                            color,
                            fillColor = color,
                            strokeWidth = 1,
                            opacity = 1,
                            fillOpacity = opacity,
                            width = 20,
                            height = width,
                            group = NULL,
                            className = 'info legend leaflet-control',
                            data = leaflet::getMapData(map),
                            ...
) {
  if (missing(shape)) {
    shape <- availableShapes()[['default']]
  }
  values <- sort(unique(as.factor(parseValues(values, data))))
  if ( length(levels(values)) > length(shape) ) {
    stop('values has more factor levels than shape. Maximum levels is 7')
  }
  shape <- shape[values]
  if ( missing(color) ) {
    stopifnot( missing(color) & !missing(pal))
    colors <- pal(values)
  } else {
    stopifnot(length(color) == 1 || length(color) == length(values))
    colors <- color
  }
  if ( missing(fillColor) ) {
    if ( !missing(pal) ) {
      fillColors <- pal(values)
    } else {
      fillColors <- colors
    }
  } else {
    stopifnot(length(fillColor) == 1 || length(fillColor) == length(values))
    fillColors <- fillColor
  }
  symbols <- Map(makeSymbol,
                 shape = shape,
                 width = width,
                 height = height,
                 color = colors,
                 fillColor = fillColors,
                 opacity = opacity,
                 fillOpacity = fillOpacity,
                 `stroke-width` = strokeWidth)
  addLegendImage(map, images = symbols,
                 labels = as.character(values),
                 title = title, labelStyle = labelStyle,
                 orientation = orientation, width = width, height = height,
                 group = group, className = className, ...)
}

#' Add a legend with Awesome Icons
#'
#' @param map
#'
#' a map widget object created from 'leaflet'
#'
#' @param iconSet
#'
#' a named list from \link[leaflet]{awesomeIconList}, the names will be the
#' labels in the legend
#'
#' @param title
#'
#' the legend title, pass in HTML to style
#'
#' @param labelStyle
#'
#' character string of style argument for HTML text
#'
#' @param marker
#'
#' whether to show the marker or only the icon
#'
#' @param orientation
#'
#' stack the legend items vertically or horizontally
#'
#' @param group
#'
#' group name of a leaflet layer group
#'
#' @param className
#'
#' extra CSS class to append to the control, space separated
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
#' library(leaflet)
#' data(quakes)
#' iconSet <- awesomeIconList(
#'   `Font Awesome` = makeAwesomeIcon(icon = "font-awesome", library = "fa",
#'                                    iconColor = 'gold', markerColor = 'red',
#'                                    spin = FALSE,
#'                                    squareMarker = TRUE,
#'                                    iconRotate = 30,
#'   ),
#'   Ionic = makeAwesomeIcon(icon = "ionic", library = "ion",
#'                           iconColor = '#ffffff', markerColor = 'blue',
#'                           spin = TRUE,
#'                           squareMarker = FALSE),
#'   Glyphicon = makeAwesomeIcon(icon = "plus-sign", library = "glyphicon",
#'                               iconColor = 'rgb(192, 255, 0)',
#'                               markerColor = 'darkpurple',
#'                               spin = TRUE,
#'                               squareMarker = FALSE)
#' )
#' leaflet(quakes[1:3,]) %>%
#'   addTiles() %>%
#'   addAwesomeMarkers(lat = ~lat,
#'                     lng = ~long,
#'                     icon = iconSet) %>%
#'   addLegendAwesomeIcon(iconSet = iconSet,
#'                        orientation = 'horizontal',
#'                        title = htmltools::tags$div(
#'                          style = 'font-size: 20px;',
#'                          'Awesome Icons'),
#'                        labelStyle = 'font-size: 16px;') %>%
#'   addLegendAwesomeIcon(iconSet = iconSet,
#'                        orientation = 'vertical',
#'                        marker = FALSE,
#'                        title = htmltools::tags$div(
#'                          style = 'font-size: 20px;',
#'                          'Awesome Icons'),
#'                        labelStyle = 'font-size: 16px;')
addLegendAwesomeIcon <- function(map,
                                 iconSet,
                                 title = NULL,
                                 labelStyle = '',
                                 orientation = c('vertical', 'horizontal'),
                                 marker = TRUE,
                                 group = NULL,
                                 className = 'info legend leaflet-control',
                                 ...) {
  stopifnot(inherits(iconSet, 'leaflet_awesome_icon_set'))
  stopifnot( !is.null(names(iconSet)) &&
               length(names(iconSet)) == length(iconSet) )
  orientation <- match.arg(orientation)
  if ( orientation == 'vertical' ) {
    wrapElements <- htmltools::tags$div
  } else {
    wrapElements <- htmltools::tags$span
  }
  currentDepNames <- vapply(map$dependencies, getElement, name = 'name',
                            FUN.VALUE = 'character')
  iconLibraries <- unique(vapply(iconSet, getElement, name = 'library',
                                 FUN.VALUE = character(1)))
  verifyIconLibrary(iconLibraries)
  iconLibraries <- c(
    fa = 'fontawesome',
    ion = 'ionicons',
    glyphicon = 'bootstrap')[iconLibraries]
  missingDeps <- setdiff(c('leaflet-awesomemarkers', iconLibraries),
                         currentDepNames)
  iconDeps <- c(
    `leaflet-awesomemarkers` = leafletAwesomeMarkersDependencies(),
    fontawesome = leafletAmFontAwesomeDependencies(),
    ion = leafletAmIonIconDependencies(),
    bootstrap = leafletAmBootstrapDependencies())[missingDeps]
  map$dependencies <- c(map$dependencies, unname(iconDeps))
  htmlElements <-
    Map(icon = iconSet,
        label = names(iconSet),
        f = function(icon, label) {
          markerClass <- ''
          if ( marker ) {
            markerClass <- sprintf(
              'awesome-marker-icon-%s awesome-marker %s',
              icon[['markerColor']],
              ifelse(icon[['squareMarker']], 'awesome-marker-square', ''))
          }
      htmltools::tagList(
        wrapElements(
        htmltools::tags$div(
          style = 'vertical-align: middle; display: inline-block; position: relative;',
              class = markerClass,
              htmltools::tags$i(class = sprintf('%1$s %1$s-%2$s %3$s',
                                                icon[['library']],
                                                icon[['icon']],
                                                ifelse(icon[['spin']], 'fa-spin', '')),
                                style = sprintf('color: %s; %s; margin-right: 0px', icon[['iconColor']],
                                                ifelse(icon[['iconRotate']] == 0, '',
                                                       sprintf('-webkit-transform: rotate(%1$sdeg);-moz-transform: rotate(%1$sdeg);-o-transform: rotate(%1$sdeg);-ms-transform: rotate(%1$sdeg);transform: rotate(%1$sdeg);',
                                                               icon[['iconRotate']]))
                                ),
                                if ( !is.null(icon[['text']]) ) {
                                  icon[['text']]
                                  }
              )
        ),
        htmltools::tags$span(label, style = sprintf('%s', labelStyle))
      )
      )
    })
  htmlElements <- addTitle(title = title, htmlElements = htmlElements)
  leaflegendAddControl(map, html = htmltools::tagList(htmlElements),
                       className = className, group = group, ...)
}

leaflegendAddControl <- function(map,
                                 html,
                                 className,
                                 group,
                                 ...) {

  if ( !is.null(group) ) {
    leafLegendClassName <- paste('leaflegend-group', gsub('\\W', '', group),
                                 sep = '-')
    className <- paste(className, leafLegendClassName)

    lf <- leaflet::addControl(map, html = html, className = className, ...)
    htmlwidgets::onRender(lf, "
function(el, x) {
  var updateLeafLegend = function() {
    var controlGroups = document.querySelectorAll(
      'input.leaflet-control-layers-selector');
    controlGroups.forEach(g => {
      var groupName = g.nextSibling.innerText.substr(1);
      var className = 'leaflegend-group-' +
        groupName.replace(/[^a-zA-Z0-9]/g, '');
      var checked = g.checked;
      document.querySelectorAll('.legend.' + className).forEach(l => {
        l.hidden = !checked;
      })
    })
  }

  updateLeafLegend();
  this.on('baselayerchange', el => updateLeafLegend())
  this.on('overlayadd', el => updateLeafLegend());
  this.on('overlayremove', el => updateLeafLegend());
}
                        ")
  } else {
    leaflet::addControl(map, html = html, className = className, ...)
  }
}

parseValues <- function(values, data) {
  if ( inherits(values, 'formula') ) {
    stopifnot(!is.null(data))
    leaflet::evalFormula(values, data)
  } else {
    values
  }
}

#' Available shapes for map symbols
#'
#' @return
#'
#' list of available shapes
#'
#' @export
#'
availableShapes <- function() {
  list(
    'default' =
      c('rect', 'circle', 'triangle', 'plus', 'cross', 'diamond', 'star',
        'stadium', 'line', 'polygon'),
    'pch' =
      c('open-rect', 'open-circle', 'open-triangle', 'simple-plus',
        'simple-cross', 'open-diamond', 'open-down-triangle', 'cross-rect',
        'simple-star', 'plus-diamond', 'plus-circle', 'hexagram', 'plus-rect',
        'cross-circle', 'triangle-rect', 'solid-rect', 'solid-circle-md',
        'solid-triangle', 'solid-diamond', 'solid-circle-bg', 'solid-circle-sm',
        'circle', 'rect', 'diamond', 'triangle', 'down-triangle'
      )
  )
}

# Borrowed from "leaflet" package internal functions
leafletAmBootstrapDependencies <- function() {
  list(htmltools::htmlDependency(
    "bootstrap", "3.3.7", "htmlwidgets/plugins/Leaflet.awesome-markers",
    package = "leaflet", script = c("bootstrap.min.js"),
    stylesheet = c("bootstrap.min.css")))
}
leafletAmFontAwesomeDependencies <- function() {
  list(htmltools::htmlDependency(
    "fontawesome", "4.7.0", "htmlwidgets/plugins/Leaflet.awesome-markers",
    package = "leaflet", stylesheet = c("font-awesome.min.css")))
}
leafletAmIonIconDependencies <- function() {
  list(htmltools::htmlDependency(
    "ionicons", "2.0.1", "htmlwidgets/plugins/Leaflet.awesome-markers",
    package = "leaflet", stylesheet = c("ionicons.min.css")))
}
leafletAwesomeMarkersDependencies <- function() {
  list(htmltools::htmlDependency(
    "leaflet-awesomemarkers",
    "2.0.3", "htmlwidgets/plugins/Leaflet.awesome-markers",
    package = "leaflet", script = c("leaflet.awesome-markers.min.js"),
    stylesheet = c("leaflet.awesome-markers.css")))

}
verifyIconLibrary <- function(library) {
  bad <- library[!(library %in% c("glyphicon", "fa", "ion"))]
  if (length(bad) > 0) {
    stop("Invalid icon library names: ", paste(unique(bad), collapse = ", "))
  }
}
