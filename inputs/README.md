# SDG Atlas 2018 data sources

The majority of the data for the Atlas comes from the World Development Indicators, and
is accessed directly using the World Bank Data API via the `wbstats` R package
and our custom internal wrapper of that `wbgdata()`. For release, flags are set in
this function to use cached/frozen versions of API data (stored in `cached_api_data`).
These can be removed per the comments in each call to access the live (but possibly revised) data.

However, a substantial minority of the figures use data from other sources, including
other World Bank datasets, datasets published by other international organizations,
and academic datasets.

These other datasets are archived within these folders, by chapter / goal.

All data from the World Bank API, and other data from World Bank sources is covered
by the World Bank's default CC-BY 4.0 license, unless otherwise specified.
In cases of external data sets, refer to the source links provided in
relevant README.md in each `sdgXX` folder in `inputs/` to establish terms of use.

You should also consult the source line in the relevant figure (either in the code
block, or the PDF publication), which will usually give additional high level 
source information (e.g. author names or a related publication link).

A small number of datasets related to goal 7 and goal 13 are restricted from
distribution. These have been removed from the `restricted/` subfolder, and
you will not be able to rebuild the figures that rely on these datasets.

For further information on these datasets in particular, contact any of:

Tariq Khokhar <tkhokhar@worldbank.org>

Andrew Whitby <awhitby@worldbank.org>

Umar Serajuddin <userajuddin@worldbank.org>