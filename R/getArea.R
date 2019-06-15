# Function to get the sum of the area of SpatialPolygons
getArea <- function(x) {
  getAreaPolygons <- function(x) {
    holes <- unlist(lapply(x@Polygons, function(x) x@hole))
    areas <- unlist(lapply(x@Polygons, function(x) x@area))
    area <- ifelse(holes, -1, 1) * areas
    area
  }
  sum(unlist(lapply(x@polygons, getAreaPolygons)))
}
