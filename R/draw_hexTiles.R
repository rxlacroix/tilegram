#' Draw hexagon tiles
#'
#' Draw a grid of tessellated hexagons over the bounding box of a SpatialPolygons object
#'
#' @param x An sp object
#' @param cellsize The size of the hexagons, if left blank then will take the average area of the polygons in the SpatialPolygons data.frame
#'
#' @return A SpatialPolygonsDataFrame of tessellated hexagons covering the bounding box of a SpatialPolygons
#' @export
#'
#' @examples
#' require(rworldmap);
#' data("countriesCoarseLessIslands") #  Load simple map without islands
#' afr <- countriesCoarseLessIslands[which(!is.na(countriesCoarseLessIslands@data$REGION) &
#'                                          countriesCoarseLessIslands@data$REGION=="Africa"),]
#' afr <- sp::spTransform(afr, CRS("+init=EPSG:32663")) # Project to equidistant grid
#' plot(hex_tiles(afr)[afr,]) # Clip to original shape and plot

draw_hexTiles <- function(area, offset_x_start=0, offset_x_end=4, offset_y_start=0, offset_y_end =4){
  grid <- expand.grid(offset_x_start:offset_x_end, offset_y_start:offset_y_end)
  grid$tessellate <- grid[,2] %% 2 == 0

  hexes <- sp::SpatialPolygons(lapply(1:nrow(grid), function(i){
    draw_hex(area, offset_x = grid[i,1], offset_y = grid[i,2], id =i, tessellate = grid[i,3])

  }))

  names(grid) <- c("offset_x", "offset_y", "tessellate")

  grid <- data.frame(id = 1:nrow(grid),grid)

  sp::SpatialPolygonsDataFrame(hexes, grid)
}
