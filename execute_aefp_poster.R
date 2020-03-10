## ---------------------------
## Script name: 
##
## Purpose of script:
##
## Date Created: 2020-03-04
##
## Date Modified: 
## 
## Created by: 
##
## ---------------------------
## Notes:
##   - Runtime is ~ 90 minutes on my machine
##
## ---------------------------

# Load required packages
req_packages <- c("tidyverse", "educationdata", "tidycensus", "lfe", "tidycensus",
                  "huxtable", "dotwhisker", "cowplot", "patchwork", "segregation")
new_packages <- req_packages[!(req_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
rm(list = c("new_packages", "req_packages"))


source("generate_ca_district_dataset.R")
source("create_ca_shapefiles.R")

rmarkdown::render("aefp_poster.Rmd")