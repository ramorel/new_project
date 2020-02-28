## ---------------------------
## Script name: 
##
## Purpose of script: Prep shapefiles for charter/threat analysis
##
## Date Created: 2020-02-19
##
## Date Modified: 
## 
## Created by: 
##
## ---------------------------
## Notes:
##   
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
         filter(statefp %in% c(4, 6, 8, 26, 41)) %>% 
         mutate(year = .y) %>% 
         select(geoid, statefp, year, geometry)
       )

save.image("district_shapefiles.RData")