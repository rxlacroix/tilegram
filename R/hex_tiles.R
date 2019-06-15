#' Compute hex_tiles
#' @export

hex_tiles <- function(x, cellsize = NULL) {
  if (is.null(cellsize)) cellsize <- getAvgArea(x)*0.6

  b <- sp::bbox(x)
  dx <- b["x", "max"] - b["x", "min"]
  dy <- b["y", "max"] - b["y", "min"]

  C <- hex_side(cellsize)
  A <- sin(deg2rad(30)) * C
  B <- sin(deg2rad(60)) * C

  hexAcross <- ceiling(dx / (B * 2))
  hexUp <- ceiling(dy / ((A + C)))

  offset_x_start <- floor(b["x", "min"] / (B * 2))
  offset_y_start <- floor(b["y", "min"] / ((A + C)))
  offset_x_end <- offset_x_start + hexAcross
  offset_y_end <- offset_y_start + hexUp

  hex_grid <- draw_hexTiles(cellsize, offset_x_start, offset_x_end, offset_y_start, offset_y_end)
  sp::proj4string(hex_grid) <- sp::proj4string(x)
  return(hex_grid)
}
