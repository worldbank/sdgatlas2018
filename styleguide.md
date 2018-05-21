# Style Guide for Final Figure Checks

For each figure, we need to check it visually and also check the code. For each of those checks, there is a general section and then possibly a figure-type specific section.

All of these rules are meant to be occasionally broken.

## Visual

### General

#### Visibility
- [ ] no text or data cut off at edges

#### Styles
- [ ] all text in Avenir Book.
- [ ] no bold text anywhere in figure, except Title
- [ ] standard colors for regions, income groups, rural/urban and gender

#### Chart elements
- [ ] has gridlines for dominant (value) axis, not for years or countries or regions axis (generally), both for scatter
- [ ] Axis text labels (e.g. region names) are close enough to plot area / data points.
- [ ] Axis value (not year) labels have commas if numbers > 1,000 (`ones()` does this if no scaling needed)

#### Captions
- [ ] title, note and source end in a period, unless source ends with URL - subtitle does not.
- [ ] all captions in sentence case (This is sentence case. This Is Not Sentence Case. NOR IS THIS.)
- [ ] subtitle is of form [indicator name][, by dimensions][, year] (denominator)
- [ ] subtitle denominator matches units on dominant axis (esp. thousands, millions, billions, etc.)
- [ ] subtitle year is mentioned - if year is MRV, has standard form
- [ ] note explains how a subset of countries was selected, if necessary
- [ ] source is in standard form

#### Specific text
- [ ] Low-income, Lower-middle-income, etc in titles; otherwise Low income, Lower middle income, etc on axis labels & legends
- [ ] Only familiar acronyms used without explanation: WDI, international organisations (IMF, WHO, IEA, etc.)

#### Maps
- [ ] Half page maps should be fully expanded
- [ ] Choropleth bins - continuous variable: Under 20, 20-40, 40-60, 60 and over - uses 'en dash' not hyphen
- [ ] Choropleth bins - discrete variable: Up to 19, 20-39, 40-59, 60 and over - uses 'en dash' not hyphen

## Code

### General

### Styles - geoms
- [ ] no colors hard coded - background "neutral" color (e.g. for percent cols) should be `style$colors$neutral`
- [ ] regions, income groups, rural/urban and gender use standard colors/fills
- [ ] `geom_text` - No font families, colors, sizes or  hard coded (search for "Avenir" for legacy uses). Family should be `style$family`: fix old examples where it is extracted from theme
- [ ] `geom_point` - scatters, etc - size, stroke and (potentially) shape use standards
- [ ] `geom_point` - bubble charts/maps - size relative to `gg_max_point_size`
- [ ] `geom_line`/`geom_path` - line charts - size is set, uses standards
- [ ] `geom_hline`/`geom_vline` - reference lines - size, linetype and color set using style (reference)
- [ ] `geom_other_dotplot` - uses if appropriate for line-connected-dots

### Styles - theme
- [ ] uses `style$theme()`
- [ ] uses `style$theme_legend()` for outside-plot-area legend
- [ ] uses `style$theme_scatter()` or `style$theme_x_title()` to turn on x axis title

### Scales
- [ ] for years axis, uses bracketed_breaks() to ensure limits labelled
- [ ] for nonyears axis, uses ones(), thousands(), millions() or millions() as appropriate to convert or add commas

### Text
- [ ] uses `wbg_name()` to assemble subtitle
