#' Shape file for Austria
#'
#' @examples
#' \dontrun{
#' # created from
#' austria <- get_shapes("https://geodata.ucdavis.edu/gadm/gadm3.6/shp/gadm36_AUT_shp.zip")
#' austria <- thin(austria, 0.001)
#' use_data(austria, overwrite=TRUE)
#' }
#' library(ggplot2)
#' library(dplyr)
#' data(austria)
#' austria %>% ggplot() + geom_sf()
"austria"
