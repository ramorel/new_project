s_dir <-
  get_education_data(
    level = "schools",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 2000:2015,
      fips = c(4, 6, 13, 48)
    )
  ) %>%
  select(
    -street_mailing:-phone,
    -state_leg_district_lower:-lunch_program
  ) %>% 
  mutate(
    leaid = as.numeric(leaid)
  )  %>% 
  arrange(ncessch_num, year) %>% 
  as_tibble()

s_enroll <-
  get_education_data(
    level = "schools",
    source = "ccd",
    topic = "enrollment",
    filters = list(
      year = 2000:2015,
      grade = 99,
      fips = c(4, 6, 13, 48)
    ),
    by = list("race")
  ) 

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
                  `99` = "total")
  )  %>% 
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
  select(asian, black, hispanic, other) %>% rowSums(na.rm = T)

s_dat$non_white_prop <-
  round(s_dat$non_white / s_dat$total, 3)

s_dat$school_diversity <-
  s_dat %>% 
  select(white, non_white) %>% 
  as.matrix() %>%
  vegan::diversity(index = "simpson")

s_dat$school_diversity <- round(s_dat$school_diversity, 3)

## 2) District data ----
d_dir <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 2000:2015,
      fips = c(4, 6, 13, 48)
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
        fips = c(4, 6, 13, 48)
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
      fips = c(4, 6, 13, 48)
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
    c(4, 6, 13, 48),
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
  arrange(leaid, year)

d_dat <-
  d_dir %>% 
  left_join(d_enroll) %>%
  left_join(
    d_pov %>% 
      mutate(fips = as.numeric(fips))
    ) %>%
  left_join(d_fin) %>%
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
  select(asian, black, hispanic, other) %>% rowSums(na.rm = T)

d_dat$non_white_prop <-
  round(d_dat$non_white / d_dat$total, 3)

d_dat$district_diversity <-
  d_dat %>% 
  select(white, non_white) %>% 
  as.matrix() %>%
  vegan::diversity(index = "simpson")

d_dat$district_diversity <- round(d_dat$district_diversity, 3)

save.image("charter_04_06_13_48.RData")

rm(list = ls()[!str_detect(ls(), "_dat")])

