# leaflegend 0.6.2

* Awesome icons are now centered in a marker in `addLegendAwesomeIcon`

* adding support for `text` parameter passed to `addLegendAwesomeIcon`. This allows an svg to be passed to as an icon.

# leaflegend 0.6.1

* patching examples that use png files from leaflet js site since they have moved.

* adding leaf images png files for examples

# leaflegend 0.6.0

* updated layers control to handle special characters in group name. All non-alphanumeric characters are removed from the class names and 
javascript selectors.

* added better error message for missing color and pal

* adding `addLegendLine` to add height only encoding of size based on values

# leaflegend 0.5.0

* updated example in README

* adding addLegendAwesomeIcon function to produce legends with markers from 
awesome icon libraries

* adding in line and polygon symbol and adding symbols to the README

# leaflegend 0.4.0

* fixes error on makeSizeLegend where fillColor is not evaluated if no argument is provided

* adding colorValues as an argument to makeSizeLegend so that symbols can be sized and colored on different vectors of data

* fixes warning on addLegendNumeric where the shape default was a vector not a single value

* adding number formatting to addSizeLegend

* adding group layer support for legends. Use addLayersControl to turn on/off
legends

* added example for using raster images with size encodings based on data


# leaflegend 0.3.0

* stroke outlines of shapes are now padded so that the stroke is not cut off

* numeric legends now have appropriate sizing for text

* star symbol outline has been fixed

* new function `makeSizeIcons` as a convenience wrapper size scaled symbols

# leaflegend 0.2.0

* new functions `addLegendSize`, `sizeNumeric`, and `sizeBreaks` were added to allow encoding size on symbols.

* `addLegendImage` supports multiple height and width parameters for images where you want different sizes

* `addLegendImage` now supports using an svg URI from the output of `makeSymbol`.
To supply a custom svg URI, add the 'svgURI' class to the character vector.

* added more shapes to `makeSymbol`, `addLegendNumeric`, 
`addLegendQuantile`, `addLegendFactor`, `addLegendBin`

* Control the opacity of the legend shapes for `addLegendNumeric`, 
`addLegendQuantile`, `addLegendFactor`, `addLegendBin`

* `makeSymbol` now returns embeddable svg

# leaflegend 0.1.0

* Added a `NEWS.md` file to track changes to the package.
