addDefaultLegend <- function(map,
                         pal,
                         values,
                         title = NULL,
                         #fontStyle = 'font-size: 24px; vertical-align: middle;',
                         #titleStyle = 'font-size: 28px; font-weight: bold; padding: 5px;',
                         #height = 24,
                         #width = 24,
                         #shape = c('rect', 'circle', 'triangle'),
                         #decreasing = FALSE,
                         ...) {
  shape <- match.arg(shape)
  html <- switch(attr(pal, 'colorType'),
    'numeric' = {
      addNumericLegend(map, pal, values, title, ...)
      },
    'quantile' = {
      probs <- attr(pal, 'colorArgs')[['probs']]
      breaks <- quantile(x = values, probs = probs, na.rm = TRUE)
      labels <- as.character(sort(
        unique(
          cut(values, breaks, include.lowest = TRUE, right = FALSE)
        )
      ))
      labels <- sprintf(' %.0f%% - %.0f%% %s', 100 * probs[-length(probs)], 100 * probs[-1], labels)
      colors <- unique(pal(sort(values)))
      Map(f = makeSVG,
          shape = shape,
          label = labels,
          color = colors,
          fontStyle = fontStyle,
          height = height,
          width = width)
      },
    'bin' = {
      sigdigits <- 3
      bins <- signif(attr(pal, 'colorArgs')[['bins']], sigdigits)
      labels <- sprintf(' %s-%s', bins[-length(bins)], bins[-1])
      colors <- unique(pal(sort(values)))
      Map(f = makeSVG,
          shape = shape,
          label = labels,
          color = colors,
          fontStyle = fontStyle,
          height = height,
          width = width)
      },
    'factor' = {
      labels <- sprintf(' %s', sort(unique(values)))
      colors <- unique(pal(sort(values)))
      Map(f = makeSVG,
          shape = shape,
          label = labels,
          color = colors,
          fontStyle = fontStyle,
          height = height,
          width = width)
      },
    stop('Invalid palette function. Create a palette with leaflet palette generator.')
  )
  if ( decreasing & attr(pal, 'colorType') != 'numeric' ) {
    html <- rev(html)
  }
  if ( !is.null(title) ) {
    html <- append(html, list(htmltools::div(style = titleStyle, title)), after = 0)
  }
  addControl(map, html = htmltools::tagList(html), ...)
}

addLegendImage <- function(map,
                           images,
                           labels,
                           title = '',
                           fontStyle = 'font-size: 24px; vertical-align: middle;',
                           titleStyle = 'font-size: 28px; font-weight: bold; padding: 5px;',
                           height = 20,
                           width = 20,
                           ...) {
  stopifnot(length(images) == length(labels))
  htmlElements <- Map(
    img = images,
    label = labels,
    f =
      function(img, label) {
        fileExt <- tolower(sub('.+(\\.)([a-zA-Z]+)', '\\2', img))
        stopifnot(fileExt %in% c('png', 'jpg', 'jpeg'))
        htmltools::tags$div(
          htmltools::tags$img(
            src = sprintf(
              'data:image/%s;base64,%s',
              fileExt,
              base64enc::base64encode(img)
            ),
            style = 'vertical-align: middle; padding: 5px;',
            height = height,
            width = width
          ),
          htmltools::tags$span(label, style = fontStyle)
        )
      }
  )
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(style = titleStyle, title)), after = 0)
  }
  addControl(map, html = htmltools::tagList(htmlElements), ...)
}

makeSVG <- function(shape, label, color, labelStyle, height, width) {
  shapeTag <- switch(
    shape,
    'rect' = htmltools::tags$rect(
      height = height,
      width = width,
      style = sprintf('fill: %s;', color)
    ),
    'circle' = htmltools::tags$circle(
      cx = height / 2,
      cy = height / 2,
      r = height / 2,
      style = sprintf('fill: %s;', color)
    ),
    'triangle' = htmltools::tags$polygon(
      points = sprintf('0,%d %d,%d %d,0', height, width, height, width / 2),
      style = sprintf('fill: %s;', color)
    ),
    stop('Invalid shape argument.')
  )
  htmltools::tags$svg(
    width = width,
    height = height,
    style = "vertical-align: middle; padding: 1px;",
    shapeTag,
    htmltools::tags$span(label, style = labelStyle),
    htmltools::tags$br()
  )
}

addLegendNumeric <- function(map,
                             pal,
                             values,
                             bins = 7,
                             labFormat = function(x) {prettyNum(x, format = 'f', big.mark = ',', scientific = FALSE)},
                             title = NULL,
                             titleStyle = 'font-size: 28px; font-weight: bold; padding: 5px;',
                             #shape = c('rect', 'stadium'),
                             height = 100,
                             width = 20,
                             tickLength = 4,
                             tickWidth = 1,
                             decreasing = FALSE,
                             ...) {
  stopifnot( attr(pal, 'colorType') == 'numeric' )
  rng <- range(values, na.rm = TRUE)
  bins <- 7
  breaks <- pretty(values, bins)
  if ( breaks[1] < rng[1] ) {
    breaks[1] <- rng[1]
  }
  if ( breaks[length(breaks)] > rng[2] ) {
    breaks[length(breaks)] <- rng[2]
  }
  colors <- pal(breaks)
  scaledbreaks <- scales::rescale(breaks)
  offsets <- sprintf('%f%%', scaledbreaks * 100)
  invisible(lapply(c('x1', 'y1', 'x2', 'y2'), assign, 0, pos = environment()))
  vertical <- height >= width
  outer <- c(1, length(breaks))
  labels <- breaks[-outer]
  if ( vertical & decreasing ) {
    y1 <- 1
    labels <- rev(labels)
  } else if ( vertical & !decreasing ) {
    y2 <- 1
  } else if ( !vertical & decreasing ) {
    x1 <- 1
    labels <- rev(labels)
  } else {
    x2 <- 1
  }
  labels <- labFormat(labels)
  textSpace <- max(strwidth(labels, units = 'inches', cex = 1.4)) * 72
  padLabel <- 5
  htmlElements <- list(htmltools::tags$svg(width = width + tickLength + padLabel + textSpace, height = height,
                           htmltools::tags$def(
                             htmltools::tags$linearGradient(
                               id = 'grad1',
                               x1 = x1, y1 = y1, x2 = x2, y2 = y2,
                               htmltools::tagList(Map(htmltools::tags$stop,
                                                      offset = offsets,
                                                      'stop-color' = colors))
                             )
                           ),
                           htmltools::tags$g(
                             htmltools::tags$rect(height = height, width = width, fill = 'url(#grad1)')
                           ),
                           Map(htmltools::tags$line,
                               x1 = width,
                               x2 = width + tickLength,
                               y1 = scaledbreaks[-outer] * height,
                               y2 = scaledbreaks[-outer] * height,
                               'stroke-width' = tickWidth,
                               stroke = 'black'
                           ),
                           Map(htmltools::tags$text,
                               #style = labelStyle,
                               labFormat(labels),
                               dx = width + tickLength + padLabel,
                               y = scaledbreaks[-outer] * height,
                               'text-anchor' = 'start',
                               dy = '.5ex'
                           )
  )
  )
  if ( !is.null(title) ) {
    htmlElements <- append(htmlElements, list(htmltools::div(style = titleStyle, title)), after = 0)
  }
  addControl(map, html = htmltools::tagList(htmlElements), ...)
}

addLegendQuantile <- function(map,
                              pal,
                              values,
                              title = NULL,
                              labelStyle = 'font-size: 24px; vertical-align: middle;',
                              titleStyle = 'font-size: 28px; font-weight: bold; padding: 5px;',
                              shape = c('rect', 'circle', 'triangle'),
                              height = 24,
                              width = 24,
                              ...) {
  stopifnot( attr(pal, 'colorType') == 'quantile' )
  shape <- match.arg(shape)
  probs <- attr(pal, 'colorArgs')[['probs']]
  breaks <- quantile(x = values, probs = probs, na.rm = TRUE)
  labels <- as.character(sort(unique(
    cut(values, breaks, include.lowest = TRUE, right = FALSE)
  )))
  labels <-
    sprintf(' %.0f%% - %.0f%% %s', 100 * probs[-length(probs)], 100 * probs[-1], labels)
  colors <- unique(pal(sort(values)))
  htmlElements <- Map(
    f = makeSVG,
    shape = shape,
    label = labels,
    color = colors,
    labelStyle = labelStyle,
    height = height,
    width = width
  )
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(style = titleStyle, title)), after = 0)
  }
  addControl(map, html = htmltools::tagList(htmlElements), ...)
}

addLegendBin <- function(map,
                         pal,
                         values,
                         title = NULL,
                         labelStyle = 'font-size: 24px; vertical-align: middle;',
                         titleStyle = 'font-size: 28px; font-weight: bold; padding: 5px;',
                         shape = c('rect', 'circle', 'triangle'),
                         height = 24,
                         width = 24,
                         ...) {
  stopifnot( attr(pal, 'colorType') == 'bin' )
  shape <- match.arg(shape)
  bins <- prettyNum(attr(pal, 'colorArgs')[['bins']], format = 'f', big.mark = ',', scientific = FALSE)
  labels <- sprintf(' %s - %s', bins[-length(bins)], bins[-1])
  colors <- unique(pal(sort(values)))
  htmlElements <- Map(f = makeSVG,
      shape = shape,
      label = labels,
      color = colors,
      labelStyle = labelStyle,
      height = height,
      width = width)
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(style = titleStyle, title)), after = 0)
  }
  addControl(map, html = htmltools::tagList(htmlElements), ...)
}

addLegendFactor <- function(map,
                            pal,
                            values,
                            title = NULL,
                            labelStyle = 'font-size: 24px; vertical-align: middle;',
                            titleStyle = 'font-size: 28px; font-weight: bold; padding: 5px;',
                            shape = c('rect', 'circle', 'triangle'),
                            height = 24,
                            width = 24,
                            ...) {
  stopifnot( attr(pal, 'colorType') == 'factor' )
  shape <- match.arg(shape)
  labels <- sprintf(' %s', sort(unique(values)))
  colors <- pal(sort(unique(values)))
  htmlElements <- Map(f = makeSVG,
      shape = shape,
      label = labels,
      color = colors,
      labelStyle = labelStyle,
      height = height,
      width = width)
  if (!is.null(title)) {
    htmlElements <-
      append(htmlElements, list(htmltools::div(style = titleStyle, title)), after = 0)
  }
  addControl(map, html = htmltools::tagList(htmlElements), ...)
}
