# SDG Atlas 2018 code for analysis & figures

This respository contains code and some data for the World Bank's Atlas of Sustainable
Development Goals 2018.

If you simply want to understand how a particular figure was derived, what
transformations were made and with what assumptions, it is probably easiest to
just browse the code on Github. To help navigate the repo, see 'Contents' below.

If you want to be able to rebuild some or all of the figures, see 'Installation'.

## Note on non-R maps

Fifteen more complex maps (rasters & other detail) were produced using QGIS. These
are not included in this repository due to the large file sizes associated with
raster map data.

They can be found in the World Bank Data Catalogue, along with an archive of this
respository, at:

TODO

## Contents

### *.R

Each chapter has an R script associated with it. Figures within the chapter are
self contained functions, with names that (usually) match the rendered figure's
filename.

With in each file, `make_all()` will re-render PNGs in the default style and location.

`make_all()` in `make.R` will re-render all the figures.

### docs/

Contains the rendered figures.

### inputs/

Contains any data which was not available via the World Bank or another API.

## Installation

A complete installation / replication guide is given in [INSTALL.md](INSTALL.md).

This repository depends on the [`wbgviz`](https://github.com/worldbank/sdgatlas2018) custom R packages.

Some parts depend a submodule of restricted data that we cannot make public.

https://github.com/worldbank/sdgatlas2018-restricted/

### To clone the restricted submodule as well (World Bank users)

To clone, first clone as usual then within the git folder hierarchy, then to get the submodules run

`git submodule update --init`

Alternatively to clone the repo including submodules in one go, you can try the `--recursive` flag:

`git clone --recursive`

## Errata

### Without history

We have corrected a small number of errors between finalization of the print publication and release of this repository. The history of these errata is not present in repository. Any further errata will be corrected as new commits.

#### docs/sdg4/fig_sdg4_school_facilities.png

Data for this figure are not currently published.

#### docs/sdg8/fig_sdg8_account_map2.png

Data were revised prior to finalization.

#### docs/sdg10/fig_sdg10_b40_explainer

Data were revised prior to finalization.

#### docs/sdg12/fig_sdg12_landfill_map.png

The data dates given in the subtitle were incorrectly listed as 2012-14.

#### docs/sdg12/fig_sdg12_recycle_compost_map

Figure updated to take a more conservative approach to handling 'no data' values.

#### docs/sdg5/fig_sdg5_domesticviolence.png

Incorrect/ambiguous source given.

### With history

None.

## Credits

Full credits for the Atlas are extensive, and included in the book/PDF. Credits for the code in this repository are as follows (stats captured when we froze changes and make public):
- @econandrew (227 commits  245,288 ++  123,955 --)
- @seladore (124 commits  6,023 ++  2,460 --)
- @tkb (120 commits  76,780 ++  5,720 --)
- @drkarthi (111 commits  310,041 ++  9,523 --)
