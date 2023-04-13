#' Show the shapefile of a country
#'
#' You have up to six guesses to identify the right country
#' @export
#' @importFrom lubridate today
#' @examples
#' if (interactive()) {
#' play_worldle()
#' }
play_worldle <- function() {
  countries <- get0("countries", envir = asNamespace("worldle"))

  today <- lubridate::today()
  set.seed(as.numeric(today))
  todays_country_id <- sample(nrow(countries), 1)

  iso_rda <- countries$iso_rda[todays_country_id]
  if (!file.exists(file.path("data", iso_rda))) {
    sf <- get_shapes(file.path("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/",countries$links[todays_country_id]))
    sf <- thin(sf, 0.001)
  }
  play(sf, countries$country.name.en[todays_country_id], attempt=1, guess="", countries$unicode.symbol[todays_country_id])
}


#' @importFrom dplyr `%>%`
#' @importFrom ggplot2 ggplot theme_void geom_sf
#' @importFrom stringr str_equal
play <- function(sf, name, attempt=1, guess="", unicode) {
  gg <- sf %>% ggplot() + geom_sf() + theme_void()
  print(gg)
  if (attempt==1)
    cat("Which country is it? ")
  else {
    cat(sprintf("%d. attempt", attempt))
  }
  guess <- scan(what=character(), n=1)
  if (str_equal(tolower(name), tolower(guess))) {
    cat(sprintf("\nYou got it in %d attempts, congratulations!", attempt))
    return()
  }
  if (attempt < 6) play(sf, name, attempt+1, guess, unicode)
  else cat(sprintf("\nSo close! It is %s!", name))
}

