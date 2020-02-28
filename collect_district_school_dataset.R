## ---------------------------
## Script name: 
##
## Purpose of script: Collect district and school level data
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
library(educationdata)

# 1) School-level data ----
s_dir <-
  get_education_data(
    level = "schools",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 2000:2015,
      fips = c(4, 6, 8, 26, 41)
    )
  ) %>%
  select(
    -street_mailing:-phone,
    -state_leg_district_lower:-lunch_program
  ) %>%
  mutate(
    leaid = as.numeric(leaid)
  ) %>%
  arrange(ncessch_num, year) %>%
  as_tibble()

s_enroll <-
  map(
    c(4, 6, 8, 26, 41), ~ get_education_data(
      level = "schools",
      source = "ccd",
      topic = "enrollment",
      filters = list(
        year = 2000:2015,
        grade = 99,
        fips = .x
      ),
      by = list("race")
    )
  )

s_enroll <- bind_rows(s_enroll)

s_enroll <-
  s_enroll %>%
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

s_dat <-
  s_dat %>%
  group_by(ncessch_num, year) %>%
  mutate(other = sum(native_am, pacific, two_or_more, na.rm = T)) %>%
  mutate_at(
    vars(asian:hispanic, white, other),
    list(prop = ~ round(. / total, 3))
  ) %>%
  ungroup()

s_dat$non_white <-
  s_dat %>%
  select(asian, black, hispanic, other) %>%
  rowSums(na.rm = T)

s_dat$non_white_prop <-
  round(s_dat$non_white / s_dat$total, 3)

# 2) District-level data ----
d_dir <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 2000:2015,
      fips = c(4, 6, 8, 26, 41)
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

d_enroll <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    filters =
      list(
        year = 2000:2015,
        grade = 99,
        fips = c(4, 6, 8, 26, 41)
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

## 3) District poverty ----
d_pov <-
  get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(
      year = 2000:2015,
      fips = c(4, 6, 8, 26, 41)
    )
  ) %>%
  mutate(leaid = as.numeric(leaid)) %>%
  as_tibble() %>%
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  arrange(leaid, year)

## 4) Finance data
d_fin <-
  map(
    c(4, 6, 8, 26, 41),
    ~ get_education_data(
      level = "school-districts",
      source = "ccd",
      topic = "finance",
      filters =
        list(
          year = 2000:2015,
          fips = .x
        )
    ) %>%
      mutate(leaid = as.numeric(leaid)) %>%
      as_tibble() %>%
      na_if(-1) %>%
      na_if(-2) %>%
      na_if(-3) %>%
      na_if(-9) %>%
      arrange(leaid, year)
  )

d_fin <-
  bind_rows(d_fin) %>%
  arrange(leaid, year) %>% 
  select(year:leaid, rev_total, rev_local_total, rev_state_total, rev_local_prop_tax, exp_total)

# 4) Disttrict-level achievement data
d_ach <- 
  get_education_data(
    level = "school-districts",
    source = "edfacts",
    topic = "assessments",
    filters = list(year = 2009:2015,
                   fips = c(4, 6, 8, 26, 41),
                   grade_edfacts = 99),
    by = list("race")) %>% 
  as_tibble() %>% 
  select(leaid, year, fips, race, contains("pct_prof")) %>% 
  mutate(
    leaid = as.numeric(leaid) %>% 
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
  pivot_wider(id_cols = c(leaid, year), names_from = race, values_from = contains("pct_prof")) %>%
  arrange(leaid, year)

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
  
  # Achievement data
  left_join(d_ach) %>% 
  
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  filter(total != 0) %>%
  as_tibble() %>%
  group_by(year, leaid) %>%
  mutate(other = sum(native_am, pacific, two_or_more, na.rm = T)) %>%
  mutate_at(
    vars(white, black, hispanic, asian, other),
    list(prop = ~ . / total)
  ) %>%
  ungroup()

d_dat$non_white <-
  d_dat %>%
  select(asian, black, hispanic, other) %>%
  rowSums(na.rm = T)

d_dat$non_white_prop <-
  round(d_dat$non_white / d_dat$total, 3)

save.image("charter_analysis.RData")
