#' Function to draw a hexagon
#'
draw_hex <- function(area=hex_area(1), offset_x = 0, offset_y = 0, id=1, tessellate=F){
  side_length <- hex_side(area)
  A <- sin(deg2rad(30)) * side_length
  B <- sin(deg2rad(60)) * side_length
  C <- side_length

  (x <- c(0, 0, B, 2*B, 2*B, B) + (offset_x*B*2) + ifelse(tessellate == T,  B, 0))
  (y <- c(A+C, A, 0, A, A+C, 2*C) + (offset_y*(A+C)))


  sp::Polygons(list(sp::Polygon(coords = matrix(c(x,y),ncol=2),hole = F)),ID=id)
}
