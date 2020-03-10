## ---------------------------
## Script name:
##
## Purpose of script: Collect district and school level data for California public schools
##
## Date Created: 2020-02-19
##
## Date Modified: 2020-03-04
##
## Created by: Richard Paquin Morel
##
## ---------------------------
## Notes:
## - Runtime for this script is ~ 30 minutes
##
## ---------------------------
library(tidyverse)
library(educationdata)

fips_to_get <- 37

# 1) School-level data for California Schools ----
# Directory data
s_dir <-
  get_education_data(
    level = "schools",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 2000:2015,
      fips = fips_to_get
    )
  ) %>%

  # Drop unneeded variables
  select(
    -street_mailing:-phone,
    -state_leg_district_lower:-lunch_program
  ) %>%

  # Make LEAID numeric so that it is consistent across datasets
  mutate(
    leaid = as.numeric(leaid)
  ) %>%
  arrange(ncessch_num, year) %>%
  as_tibble()

# School enrollment data by race
s_enroll <-
  get_education_data(
    level = "schools",
    source = "ccd",
    topic = "enrollment",
    filters = list(
      year = 2000:2015,
      grade = 99,
      fips = fips_to_get
    ),
    by = list("race")
  ) %>%
  mutate(
    leaid = as.numeric(leaid),
    race = recode(race,
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
      `99` = "total"
    )
  ) %>%
  pivot_wider(id_cols = c(ncessch_num, year), names_from = race, values_from = enrollment) %>%
  arrange(ncessch_num, year)

# Create the school level dataset by joining
s_dat <-
  left_join(
    s_dir,
    s_enroll,
    by = c("ncessch_num", "year")
  ) %>%
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  filter(total != 0) %>%
  as_tibble()

# Create "other" race category for small groups and find proportions
s_dat <-
  s_dat %>%
  group_by(ncessch_num, year) %>%
  mutate(other = sum(native_am, pacific, two_or_more, na.rm = TRUE)) %>%
  mutate_at(
    vars(asian:hispanic, white, other),
    list(prop = ~ round(. / total, 3))
  ) %>%
  ungroup()

# Create "non-white" category and find proportion within schools
s_dat$non_white <-
  s_dat %>%
  select(asian, black, hispanic, other) %>%
  rowSums(na.rm = TRUE)

s_dat <-
  s_dat %>% 
  mutate(non_white_prop = round(non_white / total, 3))

# 2) District-level data ----

# Directory
d_dir <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 2000:2015,
      fips = fips_to_get
    )
  ) %>%
  mutate(
    leaid = as.numeric(leaid)
  ) %>%
  select(
    -street_mailing:-phone,
    -state_leg_district_lower:-state_leg_district_upper
  ) %>%
  arrange(leaid, year) %>%
  as_tibble()

# Enrollment by race
d_enroll <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    filters =
      list(
        year = 2000:2015,
        grade = 99,
        fips = fips_to_get
      ),
    by = list("race")
  ) %>%
  mutate(
    leaid = as.numeric(leaid),
    race = recode(race,
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
      `99` = "total"
    )
  ) %>%
  arrange(leaid, year) %>%
  pivot_wider(id_cols = c(leaid, year), names_from = race, values_from = enrollment)

# District poverty
d_pov <-
  get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(
      year = 2000:2015,
      fips = fips_to_get
    )
  ) %>%
  mutate(leaid = as.numeric(leaid)) %>%
  as_tibble() %>%
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  arrange(leaid, year)

# District financial data
d_fin <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "finance",
    filters =
      list(
        year = 2000:2015,
        fips = fips_to_get
      )
  ) %>%
  mutate(leaid = as.numeric(leaid)) %>%
  as_tibble() %>%
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  arrange(leaid, year)

d_fin <-
  bind_rows(d_fin) %>%
  arrange(leaid, year) %>%
  select(year:leaid, rev_total, rev_fed_total, rev_local_total, rev_state_total, rev_local_prop_tax, exp_total)

# 4) Join district-level data
d_dat <-
  d_dir %>%

  # Enrollment data
  left_join(d_enroll) %>%

  # Poverty data
  left_join(
    d_pov %>%
      mutate(fips = as.numeric(fips))
  ) %>%

  # Finance data
  left_join(d_fin) %>%
  
  # Missing values encoding
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  
  # Drop districts with 0 enrollemtn
  filter(total != 0) %>%
  
  # Create other category
  group_by(year, leaid) %>%
  mutate(other = sum(native_am, pacific, two_or_more, na.rm = TRUE)) %>%
  mutate_at(
    vars(white, black, hispanic, asian, other),
    list(prop = ~ . / total)
  ) %>%
  ungroup()

# Create non-white category
d_dat$non_white <-
  d_dat %>%
  select(asian, black, hispanic, other) %>%
  rowSums(na.rm = TRUE)

d_dat <-
  d_dat %>% 
  mutate(non_white_prop = round(non_white / total, 3))

save.image("focal_state_school_data.RData")
