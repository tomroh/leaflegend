# Create Map Symbols for 'leaflet' maps

Create Map Symbols for 'leaflet' maps

## Usage

``` r
makeSymbol(
  shape,
  width,
  height = width,
  color,
  fillColor = color,
  opacity = 1,
  fillOpacity = opacity,
  label = NULL,
  ...
)

makeSvgUri(svg, width, height, strokeWidth)

makeSymbolIcons(
  shape,
  color,
  fillColor = color,
  opacity,
  fillOpacity = opacity,
  strokeWidth = 1,
  width,
  height = width,
  label = NULL,
  ...
)

addSymbols(
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
  dashArray = NULL,
  label = NULL,
  data = leaflet::getMapData(map),
  ...
)

addSymbolsSize(
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
  minSize = NULL,
  maxSize = NULL,
  data = leaflet::getMapData(map),
  ...
)

sizeNumeric(
  values,
  baseSize,
  minSize = NULL,
  maxSize = NULL,
  centerPoint = mean(values, na.rm = TRUE)
)

sizeBreaks(values, breaks, baseSize, minSize = NULL, maxSize = NULL, ...)

makeSymbolsSize(
  values,
  shape = "rect",
  color,
  fillColor,
  opacity = 1,
  fillOpacity = opacity,
  strokeWidth = 1,
  baseSize,
  minSize = NULL,
  maxSize = NULL,
  ...
)
```

## Arguments

- shape:

  the desired shape of the symbol, See
  [availableShapes](https://leaflegend.delveds.com/reference/availableShapes.md)

- width:

  in pixels

- height:

  in pixels

- color:

  stroke color

- fillColor:

  fill color

- opacity:

  stroke opacity

- fillOpacity:

  fill opacity

- label:

  a character vector of labels to display at the center of each symbol

- ...:

  arguments to pass to `pretty`

- svg:

  inner svg tags for symbol

- strokeWidth:

  stroke width in pixels

- map:

  a map widget object created from 'leaflet'

- lng:

  a numeric vector of longitudes, or a one-sided formula of the form
  `~x` where `x` is a variable in `data`; by default (if not explicitly
  provided), it will be automatically inferred from data by looking for
  a column named `lng`, `long`, or `longitude` (case-insensitively)

- lat:

  a vector of latitudes or a formula (similar to the `lng` argument; the
  names `lat` and `latitude` are used when guessing the latitude column
  from `data`)

- values:

  the values used to generate shapes; can be omitted for a single type
  of shape

- dashArray:

  a string or vector/list of strings that defines the stroke dash
  pattern

- data:

  the data object from which the argument values are derived; by
  default, it is the `data` object provided to
  [`leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  initially, but can be overridden

- baseSize:

  re-scaling size in pixels of the mean of the values, the average value
  will be this exact size

- minSize:

  minimum size in pixels of a symbol; values that would scale below this
  are clamped to `minSize`; `baseSize` must be greater than `minSize`

- maxSize:

  maximum size in pixels of a symbol; values that would scale above this
  are clamped to `maxSize`; `baseSize` must be less than `maxSize`

- centerPoint:

  the value used as the center of the scaling; defaults to the mean of
  `values`; the symbol for `centerPoint` will be exactly `baseSize`
  pixels

- breaks:

  an integer specifying the number of breaks or a numeric vector of the
  breaks; if a named vector then the names are used as labels.

## Value

HTML svg element

## Examples

``` r
library(leaflet)
data(quakes)

# symbol with a label centered inside
makeSymbol('circle', width = 24, color = 'black', fillColor = 'steelblue',
           fillOpacity = 0.8, label = 'A')
#> [1] "data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2226%22%20height%3D%2226%22%3E%0A%20%20%3Ccircle%20id%3D%22circle%22%20cx%3D%2213%22%20cy%3D%2213%22%20r%3D%2212%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%3E%3C%2Fcircle%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2214px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3EA%3C%2Ftext%3E%0A%3C%2Fsvg%3E"
#> attr(,"class")
#> [1] "character" "svgURI"   

# map markers with per-point labels
quakes$label <- as.character(seq_len(nrow(quakes)))
leaflet(quakes[1:20, ]) %>%
  addTiles() %>%
  addSymbols(lat = ~lat, lng = ~long, color = 'black',
             fillColor = 'steelblue', fillOpacity = 0.8,
             label = ~label)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addMarkers","args":[[-20.42,-20.62,-26,-17.97,-20.42,-19.68,-11.7,-28.11,-28.74,-17.47,-21.44,-12.26,-18.54,-21,-20.7,-15.94,-13.64,-17.83,-23.5,-22.63],[181.62,181.03,184.1,181.66,181.96,184.31,166.1,181.93,181.74,179.59,180.69,167,182.11,181.66,169.92,184.95,165.96,181.5,179.78,180.31],{"iconUrl":{"data":["data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E1%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E2%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E3%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E4%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E5%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E6%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E7%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E8%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E9%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E10%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E11%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E12%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E13%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E14%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E15%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E16%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E17%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E18%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E19%3C%2Ftext%3E%0A%3C%2Fsvg%3E","data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20width%3D%2222%22%20height%3D%2222%22%3E%0A%20%20%3Crect%20id%3D%22rect%22%20x%3D%221%22%20y%3D%221%22%20height%3D%2220%22%20width%3D%2220%22%20stroke%3D%22black%22%20fill%3D%22steelblue%22%20stroke-opacity%3D%221%22%20fill-opacity%3D%220.8%22%20stroke-width%3D%221%22%20stroke-dasharray%3D%22none%22%3E%3C%2Frect%3E%0A%20%20%3Ctext%20x%3D%2250%25%22%20y%3D%2250%25%22%20dominant-baseline%3D%22central%22%20text-anchor%3D%22middle%22%20font-size%3D%2212px%22%20font-family%3D%22sans-serif%22%20fill%3D%22black%22%20stroke-width%3D%220%22%3E20%3C%2Ftext%3E%0A%3C%2Fsvg%3E"],"index":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]},"iconAnchorX":11,"iconAnchorY":11},null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[-28.74,-11.7],"lng":[165.96,184.95]}},"evals":[],"jsHooks":[]}
```
