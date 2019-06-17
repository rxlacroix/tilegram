#' @import sf units lwgeom dplyr clue
NULL
#' Make a Tilegram
#'
#' @param x An sf object
#'
#' @return A sf projected to EPSG:32663 equidistant grid
#' @export
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' ncTileGram <- makeTilegram(nc)
#'
#'
#' require(GADMTools)
#' CH_districts <- gadm_sf_loadCountries("CHE", level = 2, basefile = "./")
#' CH_districts <- CH_districts$sf
#' CH_districts_tilegram <- makeTilegram(CH_districts)
#' ggplot() +
#'   geom_sf(data = CH_districts, aes(fill = NAME_1))
#'
#' ggplot() +
#'   geom_sf(data = CH_districts_tilegram, aes(fill = NAME_1))
makeTilegram <- function(sp) {
  sp$id <- 1:nrow(sp)
    sp.df <- sp
  st_geometry(sp.df) <- NULL

  sp <- st_cast(sp, to = "POLYGON")

  sp$carea <- as.numeric(st_area(sp))
  sp <- sp[order(sp$carea, decreasing = TRUE), ]


  sp <- sp[!duplicated(sp$id), ]

  sp <- sp[order(sp$id, decreasing = FALSE), ]
  row.names(sp) <- sp$id

  sp <- st_transform(sp, 32663)
  tiles <- hex_tiles(as(sp, "Spatial"))
  sp <- st_make_valid(st_as_sf(sp))
  tiles <- st_make_valid(st_as_sf(tiles))
  tiles$tile_id <- as.numeric(row.names(tiles))

  pts <- st_centroid(sp)
  pts$pt_id <- row.names(pts)
  tileCentroids <- st_centroid(tiles)
  tile_pref <- data.frame()
  if(nrow(pts) < 300){
    distance <- drop_units(st_distance(pts, tileCentroids, by_element = F))
    colnames(distance) <- tileCentroids$tile_id
    tile_pref <- t(apply(distance, 1, function(x) rank(x, ties.method = "random")))

  } else {
    for (i in 1:nrow(pts)) {
      distance <- drop_units(st_distance(pts[i, ], tileCentroids, by_element = F))
      colnames(distance) <- tileCentroids$tile_id
      y <- t(apply(distance, 1, function(x) rank(x, ties.method = "random")))
      tile_pref <- rbind(tile_pref, y)
      cat("Feature ", i, "on ", nrow(pts), " : ", (i / nrow(pts) * 100), " % \n")
    }

  }

  tile_pref <- as.matrix(tile_pref)
  solved <- clue::solve_LSAP(tile_pref, maximum = FALSE)
  solved_cols <- c(as.numeric(solved))
  tile_pref <- as.data.frame(tile_pref)
  id_solved <- vector()
  for (i in 1:nrow(tile_pref)) {
    id_solved[i] <- as.numeric(colnames(tile_pref[solved_cols[i]]))
  }

  newDat <- data.frame(tile_region = as.numeric(row.names(tile_pref)), id = id_solved, stringsAsFactors = F)
  newTiles <- tiles


  newTiles <- merge(newTiles, newDat, by.x = "tile_id", by.y = "id")

  newTiles <- merge(newTiles, sp.df, by.x = "tile_region", by.y = "id")

  return(newTiles)
}
