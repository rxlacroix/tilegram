#' The average area of SpatialPolygons
getAvgArea <- function(x){
  l <- length(x)
  avgArea <- getArea(x)/l
  return(avgArea)
}
