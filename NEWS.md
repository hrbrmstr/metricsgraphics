# metricsgraphics 0.9.0

* updated all JS libs to latest versions (including the core Metrics Graphics library)
* Incorporated the `mg-region` region annotation addon (use with `mjs_annotate_region`). 
* Borrowed the ability to add custom CSS rules to a chart (via `mjs_add_css_rule`) from [taucharts](http://github.com/hrbrmstr/taucharts).
* Also added the ability to individually color lines without resorting to CSS (a new feature in the underlying JS lib).
* Along with bare or quoted inputs, you can also now make the column names variables. 
* The vignette has also been updated to show more examples. 
* Examples that depended upon the `ggplot2::movies` data set work with the new ggplot2 which has separated out `movies` into `ggplot2movies::movies`

# metricsgraphics 0.8.5

* enhanced & changed some of the package functions to match MG's new javascript interface. 
* Added `mjs_add_confidence_band`.
* Added package vignette.

# metricsgraphics 0.8.0

* updated `mjs_rollover` to show scatterplot example
* cleaned up some of the base code & package layout for an attempt at a CRAN release. 
* updated MetricsGraphics.js to latest release version (2.5)

# metricsgraphics 0.7.5

* updated all functions that take bare inputs and made them flexible enough to take bare or quoted inputs.

# metricsgraphics 0.7.0

* Updated widget for MetricsGraphics 2.1.0
* added `mjs_add_mouseover` and tweaked `mjs_bar`. NOTE: As the Mozilla folks said, the bar charts API is quite unstable. There won't be much effort to support them in this package until the API stabilizes a bit more.

# metricsgraphics 0.6.1

* Fixed bug that broke widget in new shiny/shinydashboard context

# metricsgraphics 0.6.0

* added `mjs_grid` for `grid.arrange`-like functionality for placing multiple charts

# metricsgraphics 0.5.0

added histograms (`mjs_histogram` & `mjs_hist`)