library(tidyverse)
library(rvest)
library(sf)

urls <-
  read_html("https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries") %>%
  html_nodes("p") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("SCHOOLDISTRICT.+\\.zip") %>%
  str_subset("TL(9[0-9])", negate = T) %>%
  str_subset("TL18|TL17", negate = T) %>%
  paste0("https://nces.ed.gov", .)

urls <- rev(urls)
urls <- urls[!duplicated(str_extract(urls, "SY[0-9]{4}"))]

shapefiles <- list()
for (i in seq(urls)) {
  temp <- tempfile()
  download.file(urls[i], temp)
  shapefiles[[i]] <-
    unzip(temp) %>%
    str_subset(".shp") %>%
    read_sf() %>%
    filter_at(vars(contains("STATEFP")), ~ . %in% c("04", "08", "06", "12", "49"))
  unlink(temp)
}

save.image("district_shapefiles.RData")

years <- seq(2000, 2016, by = 2)

shapefiles <-
  shapefiles %>%
  map2(
    years,
    ~ .x %>%
      mutate(year = .y) %>%
      select(year, everything()) %>%
      st_as_sf() %>%
      st_sf(crs = 4326) %>% 
      st_transform(4326) %>% 
      lwgeom::st_make_valid()
  )

save.image("district_shapefiles.RData")