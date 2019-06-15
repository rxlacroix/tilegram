#' Get the length of a side of hexagon for a given area
hex_side <- function(area) {
  (3^0.25) * sqrt(2 * (area / 9))
}
