---
title: "metricsgraphics: An R htmlwidget interface to the MetricsGraphics.js D3-based visualization library"
authors:
 - name: Bob Rudis
   orcid: 0000-0001-5670-2640
   affiliation: Rapid7
date: 2016-04-24
bibliography: paper.bib
---

# Summary

[MetricsGraphics.js](http://metricsgraphicsjs.org/) is a library built on top of [D3](https://d3js.org/) that is optimized for visualizing and laying out time-series data. It provides a simple way to produce common types of graphics in a principled, consistent and responsive way. The library currently supports line charts, scatterplots and histograms as well as features like rug plots and basic linear regression.

The `metricsgraphics` R package wraps the MetricsGraphics.js library into an [htmlwidget](http://htmlwidgets.org/), making it easy to use in R Markdown documents and Shiny applications.

# Core Functionality

Plots are composed first by calling `mjs_plot()` with the `data.frame` containing `x` and `y` values to plot. The `x` and `y` paramters may be bare or quoted column names. This object can be passed or piped to one of the "geom"-like functions to product the desired chart. The package provides support for:

- (multiple) line charts with `mjs_line()` and `mjs_add_line()`
- bar charts with `mjs_bar()`
- histograms with `mjs_histogram()`
- scatterplots with `mjs_point()`

Additional functions for annotations and combining plots are also provided by the package along with the ability to turn simple (e.g. singe `Geom`) `ggplot2` plots automatically into corresponding `metricsgraphics` objects using `as_mjsplot()`.

# References
