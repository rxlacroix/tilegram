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
 plot(ncTileGram["NWBIR74"])

```

![](https://i.ibb.co/S7c79Nd/Screenshot-from-2019-06-15-18-19-37.png)
![](https://i.ibb.co/w7hQBX9/Screenshot-from-2019-06-15-18-20-30.png)

Swiss municipalities

![](https://i.ibb.co/ZBy2dCm/Screenshot-from-2019-06-15-19-17-37.png)
