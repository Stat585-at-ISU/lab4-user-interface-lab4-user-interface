#' Thin a shapefile
#'
#' @param x shapefile
#' @param tolerance numeric value between 0 and 1
#' @export
#' @importFrom rmapshaper ms_simplify
thin <- function(x, tolerance = 0.1){
  rmapshaper::ms_simplify(x, snap_interval = tolerance)
}
