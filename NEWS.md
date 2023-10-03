# leaflegend 1.1.10

* `addLegendNumeric` gains `labelStyle` argument and significant improvements 
to the layout that will handle larger font sizes and long text widths.

* added `stacked` argument to `addLegendSize` to allow size legends that are 
more compact when symbols are overlayed. See examples in `?addLegendSize`.

* groups can now have underscores in their name for show/hide functionality.

* added `between` argument to `addLegendBin` and `addLegendQuantile` so that 
users can change the dash.

* fixed issue where `addSymbols` and `addSymbolsSize` only worked when 
directly specifying `lat` and `lng`. These now work for sf objects. 

* added `dashArray` argument for symbols functions. The main purpose is to 
allow dashed line encodings, but all symbols can have dashed outer lines.

# leaflegend 1.1.1

* updating test for 'leaflet' changes in v2.2.0

* pch solid symbols 15-20 will use `color` if `fillColor` is missing

# leaflegend 1.1.0

* `availableShapes` is provided for convenience to look up supported symbol
names.

* `addLegendFactor`, `addLegendBin`, and `addLegendQuantile` will now show a 
symbol for missing values if they exist and gain the `naLabel` argument.

* Label spacing adjustments for `addLegendNumeric` which should support longer labels.

* `addLegendNumeric` gains an argument `naLabel` and now shows the NA color 
when there are missing values.

* `addLegendNumeric` gains an argument `labels` which allows the user to 
pass label names for numeric breaks.

* The `breaks` argument in `addLegendLine` and `addLegendSize` now allows the 
user to pass a named list where the names are the labels.

* `makeSymbol` and `makeSymbolIcons` can now create map symbols that are consistent with 
base R `pch`. The `shape` argument can accept the name or an integer 
(0-indexed). `availableShapes` provides a list of named options.

# leaflegend 1.0.0

* A new function `addLegendSymbol` has been added to automatically encode character/factors as various map symbols.

* The default argument for `baseSize` in `addLegendSize` has been changed to 20.

* `values` argument is no longer used by  `addLegendBin` since it is not 
necessary and causes problems when number of values is less than the number 
of bins.

* Updated example in README

* Documentation has been updated for `makeSymbols` and the name has been 
changed to `mapSymbols` with some additional functions added to it.

* Internal functions have been renamed to match naming conventions.

* Renamed `makeSizeIcons` to `makeSymbolsSize` to be consistent in naming 
conventions. The `colorValues` argument has been removed. For the same functionality, specify a vector of `color` and `fillColor`.

* Fixed issue with image styling missing for raster images in `addLegendImage`

* Added functions to automate the task of creating map symbols. 
`addSymbols` will create map symbols based on a character or factor vector. `addSymbolsSize` will create map symbols with a size encoding.

* Code re-factoring for `addLegendBin`, `addLegendQuantile`, and `addLegendFactor` as grouping of values for each is different but the assembly of the HTML is the same.

* Unit tests have been added to cover all the current functions.

* Gradient IDs for numeric legends are now based on the function call or data "name" in `values`.

* `makeSizeIcons` default opacity is now 1.

* Adding internal functions to add dependencies for `addLegendAwesomeIcon`. Awesome Icons would not show in legends because the needed HTML dependencies were not loaded unless `addAwesomeIcons` was included. `addLegendAwesomeIcon` will now check if the dependencies are in the "leaflet" map and include them if missing.

* Added error messages for invalid `width`, `height`, `tickWidth`, `tickLength`, and `strokeWidth` arguments. Prior to this negative values would not throw errors and the visual output of legends was not desirable.

* Ability to specify `values` parameter as a formula to retrieve from the `data` argument which is the same as the "leaflet" package.

* `makeSymbolIcons` now supports vectorization for multiple shape arguments

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
