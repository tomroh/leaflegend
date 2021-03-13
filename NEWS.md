# leaflegend 0.2.2

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
