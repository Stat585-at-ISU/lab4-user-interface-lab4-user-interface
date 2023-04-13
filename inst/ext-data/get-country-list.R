library(tidyverse)
library(rvest)
library(countrycode)

url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/"
doc <- read_html(url)
country_links <- doc %>% html_elements(css = "a") %>% html_text() %>% grep(pattern="gadm", value = TRUE)

countries <- data.frame(links=country_links) %>% mutate(
  iso3 = gsub(".*_(...)_.*", "\\1", links)
) %>% left_join(
  codelist %>% select(iso3c, unicode.symbol, country.name.en), by=c("iso3"="iso3c")
)

countries <- countries %>% mutate(
  iso_rda = gsub("\\.zip",".rda",links)
)

use_data(countries, internal = TRUE)

################
# austria object
austria <- get_shapes("https://geodata.ucdavis.edu/gadm/gadm3.6/shp/gadm36_AUT_shp.zip")
austria <- thin(austria, 0.001)
use_data(austria, overwrite=TRUE)

