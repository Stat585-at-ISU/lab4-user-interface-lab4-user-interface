#' Download a shapefile from GADM
#'
#' we need a bit more explanation here ... The function works with both versions 3.6 and 4.0, 4.1 of the GADM library
#' @param url address to zipped file shapes
#' @param level which level to extract? 0 is country outline, 1 is state/territories, for some (but not all) countries exist lower-level country divisions (e.g. counties, municipalities)
#' @importFrom sf read_sf
#' @importFrom utils download.file unzip
#' @importFrom stringr str_detect
#' @export
#' @examples
#' # url for Iran
#' url <- 'https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_IRN_shp.zip'
#' # url for Bahamas
#' url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_BHS_shp.zip"
#' country <- get_shapes(url)
#' url <- "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_BHS_shp.zip"
#' library(ggplot2)
#' library(dplyr)
#' country %>% thin(tolerance=0.001) %>%  ggplot() + geom_sf() + theme_void()
get_shapes <- function(url, level = 0) {

  stopifnot(valid_url(url)) # checks if the url is valid

  url_splitted <- strsplit(url, split = "/")[[1]]
  file_name <- url_splitted[length(url_splitted)]
  stopifnot(str_detect(file_name, "\\.zip$"))
#  browser()

  # Prepare for download
  datadir <- tempdir()
  destfile <- file.path(datadir, file_name)

  # Try to download
  download.file(url,
    destfile = destfile,
    mode = "wb",
    quiet=TRUE
  )
  # unzip
  new_folder <- gsub(".zip$", "", destfile)
  unzip(destfile,
    exdir = new_folder
  )
  # read shapefile
  # Read
  shapefile <- file.path(new_folder, gsub("shp.zip", paste0(level, ".shp"), file_name))

  sf_file <- file.path(shapefile)

  sf::read_sf(sf_file)
}


#' helper function
#'
#' check whether a url is valid
#' @param url_in character value of the website to be checkd for validity
#' @param t timeout time in seconds.
#' @returns logic value: TRUE for valid connection, FALSE for not
#' @source https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
#' @export
#' @examples
#'  valid_url("http://www.stat.iastate.edu")
#'  valid_url("http://www.not/a/good/url.de")
valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}

