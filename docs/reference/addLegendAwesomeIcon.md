# Add a legend with Awesome Icons

Add a legend with Awesome Icons

## Usage

``` r
addLegendAwesomeIcon(
  map,
  iconSet,
  title = NULL,
  labelStyle = "vertical-align: middle;",
  orientation = c("vertical", "horizontal"),
  marker = TRUE,
  group = NULL,
  className = "info legend leaflet-control",
  ...
)
```

## Arguments

- map:

  a map widget object created from 'leaflet'

- iconSet:

  a named list from
  [awesomeIconList](https://rstudio.github.io/leaflet/reference/awesomeIconList.html),
  the names will be the labels in the legend

- title:

  the legend title, pass in HTML to style

- labelStyle:

  character string of style argument for HTML text

- orientation:

  stack the legend items vertically or horizontally

- marker:

  whether to show the marker or only the icon

- group:

  group name of a leaflet layer group

- className:

  extra CSS class to append to the control, space separated

- ...:

  arguments to pass to
  [addControl](https://rstudio.github.io/leaflet/reference/map-layers.html)

## Value

an object from
[addControl](https://rstudio.github.io/leaflet/reference/map-layers.html)

## Examples

``` r
library(leaflet)
data(quakes)
iconSet <- awesomeIconList(
  `Font Awesome` = makeAwesomeIcon(icon = "font-awesome", library = "fa",
                                   iconColor = 'gold', markerColor = 'red',
                                   spin = FALSE,
                                   squareMarker = TRUE,
                                   iconRotate = 30,
  ),
  Ionic = makeAwesomeIcon(icon = "ionic", library = "ion",
                          iconColor = '#ffffff', markerColor = 'blue',
                          spin = TRUE,
                          squareMarker = FALSE),
  Glyphicon = makeAwesomeIcon(icon = "plus-sign", library = "glyphicon",
                              iconColor = 'rgb(192, 255, 0)',
                              markerColor = 'darkpurple',
                              spin = TRUE,
                              squareMarker = FALSE)
)
leaflet(quakes[1:3,]) %>%
  addTiles() %>%
  addAwesomeMarkers(lat = ~lat,
                    lng = ~long,
                    icon = iconSet) %>%
  addLegendAwesomeIcon(iconSet = iconSet,
                       orientation = 'horizontal',
                       title = htmltools::tags$div(
                         style = 'font-size: 20px;',
                         'Awesome Icons'),
                       labelStyle = 'font-size: 16px;') %>%
  addLegendAwesomeIcon(iconSet = iconSet,
                       orientation = 'vertical',
                       marker = FALSE,
                       title = htmltools::tags$div(
                         style = 'font-size: 20px;',
                         'Awesome Icons'),
                       labelStyle = 'font-size: 16px;')

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addAwesomeMarkers","args":[[-20.42,-20.62,-26],[181.62,181.03,184.1],{"icon":["font-awesome","ionic","plus-sign"],"markerColor":["red","blue","darkpurple"],"iconColor":["gold","#ffffff","rgb(192, 255, 0)"],"spin":[false,true,true],"squareMarker":[true,false,false],"iconRotate":[30,0,0],"prefix":["fa","ion","glyphicon"]},null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addControl","args":["<div>\n  <div style=\"font-size: 20px;\">Awesome Icons<\/div>\n<\/div>\n<span>\n  <div style=\"vertical-align: middle; display:&#10;          inline-block; position: relative;\" class=\"awesome-marker-icon-red awesome-marker awesome-marker-square\">\n    <i class=\"fa fa-font-awesome \" style=\"color: gold; -webkit-transform: rotate(30deg);-moz-transform: rotate(30deg);-o-transform: rotate(30deg);-ms-transform: rotate(30deg);transform: rotate(30deg);; margin-right: 0px\"><\/i>\n  <\/div>\n  <span style=\"font-size: 16px;\">Font Awesome<\/span>\n<\/span>\n<span>\n  <div style=\"vertical-align: middle; display:&#10;          inline-block; position: relative;\" class=\"awesome-marker-icon-blue awesome-marker \">\n    <i class=\"ion ion-ionic fa-spin\" style=\"color: #ffffff; ; margin-right: 0px\"><\/i>\n  <\/div>\n  <span style=\"font-size: 16px;\">Ionic<\/span>\n<\/span>\n<span>\n  <div style=\"vertical-align: middle; display:&#10;          inline-block; position: relative;\" class=\"awesome-marker-icon-darkpurple awesome-marker \">\n    <i class=\"glyphicon glyphicon-plus-sign fa-spin\" style=\"color: rgb(192, 255, 0); ; margin-right: 0px\"><\/i>\n  <\/div>\n  <span style=\"font-size: 16px;\">Glyphicon<\/span>\n<\/span>","topleft",null,"info legend leaflet-control"]},{"method":"addControl","args":["<div>\n  <div style=\"font-size: 20px;\">Awesome Icons<\/div>\n<\/div>\n<div>\n  <div style=\"vertical-align: middle; display:&#10;          inline-block; position: relative;\" class=\"\">\n    <i class=\"fa fa-font-awesome \" style=\"color: gold; -webkit-transform: rotate(30deg);-moz-transform: rotate(30deg);-o-transform: rotate(30deg);-ms-transform: rotate(30deg);transform: rotate(30deg);; margin-right: 0px\"><\/i>\n  <\/div>\n  <span style=\"font-size: 16px;\">Font Awesome<\/span>\n<\/div>\n<div>\n  <div style=\"vertical-align: middle; display:&#10;          inline-block; position: relative;\" class=\"\">\n    <i class=\"ion ion-ionic fa-spin\" style=\"color: #ffffff; ; margin-right: 0px\"><\/i>\n  <\/div>\n  <span style=\"font-size: 16px;\">Ionic<\/span>\n<\/div>\n<div>\n  <div style=\"vertical-align: middle; display:&#10;          inline-block; position: relative;\" class=\"\">\n    <i class=\"glyphicon glyphicon-plus-sign fa-spin\" style=\"color: rgb(192, 255, 0); ; margin-right: 0px\"><\/i>\n  <\/div>\n  <span style=\"font-size: 16px;\">Glyphicon<\/span>\n<\/div>","topleft",null,"info legend leaflet-control"]}],"limits":{"lat":[-26,-20.42],"lng":[181.03,184.1]}},"evals":[],"jsHooks":[]}
```
