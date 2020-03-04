## ---------------------------
## Script name: create_ca_shape
##
## Purpose of script: Prep shapefiles for charter/threat analysis of California school districts
##
## Date Created: 2020-02-19
##
## Date Modified: 2020-03-04
## 
## Created by: Richard Paquin Morel
##
## ---------------------------
## Notes:
##   - Shapefiles must be downloaded from https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries
##
## ---------------------------

library(tidyverse)
library(sf)

shp_files <- dir("shapefiles", pattern = ".shp$")

# Reorder so 1999-2000 is first
shp_files <- shp_files[c(9, 1:8)]

years <- seq(from = 1999, to = 2015, by = 2)

shapefiles <-
  map(shp_files,
      ~ read_sf(paste("shapefiles", .x, sep = "/")) %>% 
        rename_at(vars(matches("[0-9]{2}")), ~ str_remove(., "[0-9]{2}")) %>% 
        select(GEOID, STATEFP, geometry) %>% 
        mutate_at(1:2, as.numeric) %>% 
        janitor::clean_names()
  )

shapefiles <-
  map2(shapefiles, 
       years, ~ .x  %>% 
         filter(statefp == 6) %>% 
         mutate(year = .y) %>% 
         select(geoid, statefp, year, geometry)
       )

save.image("ca_district_shapefiles.RData")