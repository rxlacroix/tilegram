# tilegram

Forked version of https://gitlab.com/lajh87/makeTilegram/

For large sf datasets

-----

A tilegram is a tiled map where regions are adjusted to have the same size.

```
  devtools::install_github("rxlacroix/tilegram")
  library(tilegram)
  library(sf)
 nc <- st_read(system.file("shape/nc.shp", package="sf"))
 ncTileGram <- makeTilegram(nc)
 plot(ncTileGram["id"])

```
