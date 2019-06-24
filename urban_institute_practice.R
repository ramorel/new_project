##################################################
## Project: Public School Enrollment over time
## Script purpose: get district-level enrollment
## Date Created: 
## Last Updated:
## Author: rpm
##################################################
library(tidyverse)
library(educationdata)
library(tidycensus)

## ccd directory
dir_dat <- 
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(year = 2000:2013)
    )

dir_dat <-
  dir_dat %>% 
  arrange(leaid, year)

## ccd enrollment data
enroll_dat <- 
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    filters = list(year = 2000:2013,
                   grade = 99),
    by = list("race")
    )

## make it wide
enroll_dat <- 
  enroll_dat %>% 
  na_if(-1) %>% 
  na_if(-2) %>% 
  na_if(-3) %>% 
  select(-sex, -grade) %>% 
  mutate(race = recode(race, 
                       `1` = "white",
                       `2` = "black",
                       `3` = "hispanic",
                       `4` = "asian",
                       `5` = "native_am",
                       `6` = "pacific",
                       `7` = "two_or_more",
                       `8` = "non_resident",
                       `9` = "unknown",
                       `20` = "other",
                       `99` = "total")
         ) %>% 
  reshape2::dcast(year + fips + leaid ~ race, value.var = "enrollment") 

## district poverty data
d_pov <- 
  get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(year = c(2000:2013))
  )

## finance data
fin_dat <- 
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "finance",
    filters = list(year = 2000:2013)
    )


## Join the datasets
districts <- 
  dir_dat %>% 
  mutate(leaid = as.numeric(leaid)) %>% 
  left_join(enroll_dat, by = c("year", "fips", "leaid")) %>% 
  mutate(leaid = as.character(leaid)) %>% 
  left_join(fin_dat, by = c("year", "fips", "leaid")) %>% 
  left_join(
    d_pov %>% 
      mutate(fips = as.numeric(fips)), 
    by = c("year", "fips", "leaid")) %>% 
  na_if(-1) %>% 
  na_if(-2) %>% 
  na_if(-3) %>% 
  arrange(fips, leaid, year)

## Add state names
fips <- 
  tidycensus::fips_codes %>% 
  transmute(state = state,
            fips = as.numeric(state_code)) %>% 
  distinct()

districts <-
  districts %>% 
  left_join(fips)
            
## Save file
write_rds(districts, "ccd_district_data.rds")

dis_dat <-
  inner_join(
    dir_dat %>% 
      mutate(leaid = as.numeric(leaid)) %>% 
      select(-enrollment), 
    enroll_dat,
    by = c("year", "fips", "leaid"))


## census data by school district
geos <- c("school district (unified)", 
          "school district (elementary)", 
          "school district (secondary)")
vars <- c(tot_pop = "B01001_001", 
          tot_male_pop = "B01001_002", 
          tot_female_pop = "B01001_026",
          wh_pop = "B02001_002",
          blk_pop = "B02001_003",
          am_in_pop = "B02001_004",
          as_pop = "B02001_005",
          pac_isl_pop = "B02001_006",
          other_pop = "B02001_007",
          two_more_pop = "B02001_008",
          native_pop = "B05002_002",
          non_cit_pop = "B05001_006",
          for_pop = "B05002_013",
          for_nat_pop = "B05002_014",
          for_non_cit_pop = "B05002_015",
          for_as_pop = "B05006_047",
          for_east_as_pop = "B05006_048",
          for_latin_am_pop = "B05006_123",
          for_enter_after_2000 = "B05005_004",
          tot_age_5_17 = "B06001_003",
          tot_age_18_24 = "B06001_004",
          for_age_5_17 = "B06001_051",
          for_age_18_24 = "B06001_052",
          tot_speak_eng_only = "B06007_002",
          tot_speak_spanish = "B06007_003",
          tot_speak_other = "B06007_006",
          for_speak_spanish = "B06007_027",
          for_speak_other = "B06007_030",
          total_male_bach = "B15002_015",
          total_female_bach = "B15002_032",
          tot_blw_100_pov = "B06012_002",
          tot_btw_100_149_pov = "B06012_003",
          tot_at_above_149_pov = "B06012_004",
          for_blw_100_pov = "B06012_018",
          for_btw_100_149_pov = "B06012_019",
          for_at_above_149_pov = "B06012_020",
          med_income = "B19013_001",
          tot_welfare = "B09010_002",
          tot_pub_assist = "B19057_002")

acs_dat <- 
  map(2009:2016, 
      ~ map(unique(fips_codes$state)[1:51], 
            function(y)
              get_acs(geography = "school district (unified)",
                      variables = vars, 
                      output = "wide",
                      year  = .x,
                      state = y)
        ))
