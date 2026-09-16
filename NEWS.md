# leaflegend 1.3.0

* added `makeSymbolText` and `makeSymbolTextIcons` for building SVG text 
symbols that can be used as 'leaflet' icons. Unlike the existing `'text'` 
shape in `makeSymbol`, these take an explicit `text` argument so each 
symbol can show a different label. `fontSize` and `fontFamily` arguments 
are exposed.

* added `addText` and `addTextSize` as convenience wrappers analogous to 
`addSymbols` and `addSymbolsSize` for placing text symbols on a map.

* added `addLegendText` and `addLegendTextSize` for legend support of 
text symbols, analogous to `addLegendSymbol` and `addLegendSize`. 
`addLegendText` exposes both `fontSize` and `fontFamily`; 
`addLegendTextSize` exposes only `fontFamily` and auto-computes 
`font-size` from each break's width. `addLegendText` takes an optional 
`labels` argument; by default no labels are placed beside the text symbols. 
`addLegendTextSize` requires a single `text` string that is rendered at 
each break size.

* corrected the `...` documentation for `mapSymbols`, `legendSymbols`, and 
`addLeafLegends`. The grouped help pages now state which pass-through 
target applies to each function in the group.
# leaflegend (development version)

* Fixed legends with a `group` not matching the state of the layers control
when the map renders inside an initially hidden container, e.g. an inactive
Quarto/bslib tab or a hidden Shiny tab (#110). Legend visibility now re-syncs
after deferred rendering completes and also responds to `showGroup`/
`hideGroup`.

* Fixed `addLegendNumeric` placing ticks and labels at mirrored positions on
vertical legends so that they did not align with the color gradient when
breaks were not symmetric within the range of values (#105, #106).

* Fixed `addLegendNumeric` with `decreasing = TRUE` not reversing the color
gradient, which left labels in the opposite order of the colors.

* User supplied `labels` in `addLegendNumeric` now always pair with the bins
in ascending order of value for both orientations, e.g. for
`bins = c(1000, 3000, 6000)` and `labels = c('low', 'med', 'high')`, 'low'
labels the 1000 tick even when `decreasing = TRUE`.

# leaflegend 1.2.8

* Intermediate ticks are now allowed for horizontal orientation in 
`addLegendNumeric`.

* Added ability to add text label to symbols.

* Size encodings get two new arguments `minSize` and `maxSize` that control the 
bounds of the size scale. 

* `addLegendBin` has a new method with the `labelCutpoints` argument that 
produces a legend with labels at the tick marks e.g. a 0-10 bin has 0
labeled at the top tick mark and 10 at the bottom tick mark.

* Changing defaults for size legends so labels are vertically aligned.

* Adding text symbol see README

* Added examples for advanced symbol customization

* Fixes an issue when multi-map documents and group names are the same where 
the layer control would only work for the last map.

# leaflegend 1.2.1

* `addLegendNumeric` now has the ability to use manual tick breaks and labels 
when the `bins` argument is a numeric vector greater than length 1. This 
only applies to vertically oriented legends.

# leaflegend 1.2.0

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
