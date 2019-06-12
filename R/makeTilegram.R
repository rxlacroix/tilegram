



deg2rad <- function(deg) {(deg * pi) / (180)} # Function to convert degrees to radians (trigonemetry)

hex_side <- function(area) {(3^0.25)*sqrt(2*(area/9))} # Get the length of a side of hexagon for a given area

hex_area <- function(side) ((3*sqrt(3))/2*side) # Get the area of a hexagon given its side length

# Function to draw a hexagon
draw_hex <- function(area=hex_area(1), offset_x = 0, offset_y = 0, id=1, tessellate=F){
  side_length <- hex_side(area)
  A <- sin(deg2rad(30)) * side_length
  B <- sin(deg2rad(60)) * side_length
  C <- side_length
  
  (x <- c(0, 0, B, 2*B, 2*B, B) + (offset_x*B*2) + ifelse(tessellate == T,  B, 0))
  (y <- c(A+C, A, 0, A, A+C, 2*C) + (offset_y*(A+C)))
  
  
  sp::Polygons(list(sp::Polygon(coords = matrix(c(x,y),ncol=2),hole = F)),ID=id)
}

# Function to get the sum of the area of SpatialPolygons
getArea <-  function(x) {
  getAreaPolygons = function(x) {
    holes = unlist(lapply(x@Polygons, function(x) x@hole))
    areas = unlist(lapply(x@Polygons, function(x) x@area))
    area = ifelse(holes, -1, 1) * areas
    area
  }
  sum(unlist(lapply(x@polygons, getAreaPolygons)))
}

# The average area of SpatialPolygons
getAvgArea <- function(x){
  l <- length(x)
  avgArea <- getArea(x)/l
  return(avgArea)
}

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

hex_tiles <- function(x, cellsize=NULL){
  
  if(is.null(cellsize)) cellsize <- getAvgArea(x)*.9
  
  b <- sp::bbox(x)
  dx <- b["x", "max"] - b["x", "min"]
  dy <- b["y", "max"] - b["y", "min"]
  
  C <- hex_side(cellsize)
  A <- sin(deg2rad(30)) * C
  B <- sin(deg2rad(60)) * C
  
  hexAcross <- ceiling(dx/(B*2))
  hexUp <- ceiling(dy/((A+C)))
  
  offset_x_start <- floor(b["x", "min"]/(B*2))
  offset_y_start <- floor(b["y", "min"]/((A+C)))
  offset_x_end <- offset_x_start + hexAcross
  offset_y_end <- offset_y_start + hexUp
  
  hex_grid <- draw_hexTiles(cellsize, offset_x_start, offset_x_end, offset_y_start, offset_y_end)
  sp::proj4string(hex_grid) <- sp::proj4string(x)
  return(hex_grid)
}


#' Make a Tilegram
#'
#' Function to make a tilegram from a SpatialPolygonsDataFrame.
#' It draws a grid of hexagons over the bounding box of the SpatialPolygonsDataFrame and
#' then uses the 'Hungarian' algorithm found in the `clue` package to match hexagons to
#' Polygons by minimising the distance between the centre of the hexagon and the centroid of the polygon.
#'
#' @param sp A SpatialPolygonDataFrame
#'
#' @return A SpatialPolygonsDataFrame projected to EPSG:32663 equidistant grid
#' @export
#'
#' @examples
#' require(rworldmap);
#' data("countriesCoarseLessIslands") #  Load simple map without islands
#' afr <- countriesCoarseLessIslands[which(!is.na(countriesCoarseLessIslands@data$REGION) &
#'                                          countriesCoarseLessIslands@data$REGION=="Africa"),]
#' tileGram <- makeTilegram(afr)
#' plot(tileGram)
makeTilegram <- function(sp){
  
sp$id <- row.names(sp)

sp <- st_cast(sp, to="POLYGON")

sp$carea <- as.numeric(st_area(sp))
sp <- sp[order(sp$carea, decreasing = TRUE),]


sp <- sp[!duplicated(sp$id),]
  
sp <- st_transform(nc, 32663)
tiles <- hex_tiles(as(sp, 'Spatial'))
sp <- st_make_valid(st_as_sf(sp))
tiles <- st_make_valid(st_as_sf(tiles))
tiles <- tiles[sp, ]
pts <- st_centroid(sp) 
rm(sp)
gc()
pts$pt_id <- row.names(pts)
tileCentroids <- st_centroid(tiles)
tileCentroids$id <- row.names(tileCentroids)
tile_pref <- data.frame()
for (i in 1:nrow(pts)) {
  gc()
  distance <- drop_units(st_distance(pts[i,], tileCentroids, by_element =F))
  colnames(distance) <- tileCentroids$id
  y <- t(apply(distance,1, function(x) rank(x,ties.method ="random")))
  tile_pref <- rbind(tile_pref, y)
  cat("Feature ", i, "on ", nrow(pts), " : ", (i/nrow(pts)*100), " % \n")
}
tile_pref <- as.matrix(tile_pref)
 solved <- clue::solve_LSAP(tile_pref, maximum = FALSE)
 solved_cols <- c(as.numeric(solved))
 tile_pref <- as.data.frame(tile_pref)
 id_solved <- vector()
 for (i in 1:nrow(tile_pref)) {
   id_solved[i] <- as.numeric(colnames(tile_pref[solved_cols[i]]))
 }
 
 newDat <- data.frame(tile_region= row.names(tile_pref), id = id_solved, stringsAsFactors = F)
 newTiles <- tiles



newTiles <- full_join(newTiles, newDat, by="id")
newTiles <- newTiles[!is.na(newTiles$tile_region),]

return(newTiles)
}
