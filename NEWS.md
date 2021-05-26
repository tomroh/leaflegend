# leaflegend 0.3.1

* fixes error on makeSizeLegend where fillColor is not evaluated if no argument is provided

* adding colorValues as an argument to makeSizeLegend so that symbols can be sized and colored on different vectors of data

* fixes warning on addLegendNumeric where the shape default was a vector not a single value

* adding number formatting to addSizeLegend

# leaflegend 0.3.0

* stroke outlines of shapes are now padded so that the stroke is not cut off

* numeric legends now have appropriate sizing for text

* star symbol outline has been fixed

* new function `makeSizeIcons` as a convenience wrapper size scaled symbols

# leaflegend 0.2.0

* new functions `addLegendSize`, `sizeNumeric`, and `sizeBreaks` were added to allow encoding size on symbols.

* `addLegendImage` supports multiple height and width paramaters for images where you want different sizes

* `addLegendImage` now supports using an svg URI from the output of `makeSymbol`.
To supply a custom svg URI, add the 'svgURI' class to the character vector.

* added more shapes to `makeSymbol`, `addLegendNumeric`, 
`addLegendQuantile`, `addLegendFactor`, `addLegendBin`

* Control the opacity of the legend shapes for `addLegendNumeric`, 
`addLegendQuantile`, `addLegendFactor`, `addLegendBin`

* `makeSymbol` now returns embeddable svg

# leaflegend 0.1.0

* Added a `NEWS.md` file to track changes to the package.
