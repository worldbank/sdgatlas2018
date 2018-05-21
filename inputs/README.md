# SDG Atlas 2018 data sources

The majority of the data for the Atlas comes from the World Development Indicators, and
is accessed directly using the World Bank Data API via the `wbstats` R package
and our custom internal wrapper of that `wbgdata()`.

However, a substantial minority of the figures use data from other sources, including
other World Bank datasets, datasets published by other international organizations,
and academic datasets.

These other datasets are archived within these folders, by chapter / goal.

A small number of datasets related to goal 7 and goal 13 are restricted from
distribution. These have been removed from the restricted/ subfolder, and
you will not be able to rebuild the figures that rely on these datasets. For
further information on these datasets in particular, contact any of:

Tariq Khokhar <tkhokhar@worldbank.org>

Andrew Whitby <awhitby@worldbank.org>

Umar Serajuddin <userajuddin@worldbank.org>