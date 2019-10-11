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

## Determine threat ---
load("shapefiles_04_06_13_48.RData")

shapefiles <-
  map(
    shapefiles, 
    ~ rename_at(
      .x, 
      vars(contains("10")),
      ~ str_remove(., "10"))
  )

district_borders <-
  map(shapefiles, st_touches)

district_borders <-
  map2(
    district_borders,
    shapefiles,
    ~ map(
      .x,
      function(y)
        as.numeric(.y$GEOID[y])
    )
  )

district_borders_even <-
  map(
    seq(district_borders),
    ~ map_dfr(
      seq(district_borders[[.x]]),
      function(y)
        tibble(
          leaid = as.numeric(shapefiles[[.x]]$GEOID[y]),
          year = shapefiles[[.x]]$year[y],
          prop_theatened_white =
            d_dat$white_prop[d_dat$leaid == as.numeric(shapefiles[[.x]]$GEOID[y]) & d_dat$year == shapefiles[[.x]]$year[y]],
          prop_threatening_overall =
            sum(
              c(
                d_dat$black[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
                d_dat$hispanic[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]]
              ),
              na.rm = T
            ) /
            sum(
              d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
              na.rm = T
            ),
          prop_threatening_black =
            sum(
              c(
                d_dat$black[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]]
              ),
              na.rm = T
            ) /
            sum(
              d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
              na.rm = T
            ),
          prop_threatening_hispanic =
            sum(
              c(
                d_dat$hispanic[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]]
              ),
              na.rm = T
            ) /
            sum(
              d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
              na.rm = T
            )
        ) %>%
        mutate(
          threat_overall = prop_theatened_white * prop_threatening_overall,
          threat_black = prop_theatened_white * prop_threatening_black,
          threat_hispanic = prop_theatened_white * prop_threatening_hispanic
        )
    )
  )

district_borders_odd <-
  map(
    seq(district_borders),
    ~ map_dfr(
      seq(district_borders[[.x]]),
      function(y)
        tibble(
          leaid = as.numeric(shapefiles[[.x]]$GEOID[y]),
          year = (shapefiles[[.x]]$year[y] + 1),
          prop_theatened_white =
            d_dat$white_prop[d_dat$leaid == as.numeric(shapefiles[[.x]]$GEOID[y]) & d_dat$year == shapefiles[[.x]]$year[y]],
          prop_threatening_overall =
            sum(
              c(
                d_dat$black[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
                d_dat$hispanic[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]]
              ),
              na.rm = T
            ) /
            sum(
              d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
              na.rm = T
            ),
          prop_threatening_black =
            sum(
              c(
                d_dat$black[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]]
              ),
              na.rm = T
            ) /
            sum(
              d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
              na.rm = T
            ),
          prop_threatening_hispanic =
            sum(
              c(
                d_dat$hispanic[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]]
              ),
              na.rm = T
            ) /
            sum(
              d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == shapefiles[[.x]]$year[y]],
              na.rm = T
            )
        ) %>%
        mutate(
          threat_overall = prop_theatened_white * prop_threatening_overall,
          threat_black = prop_theatened_white * prop_threatening_black,
          threat_hispanic = prop_theatened_white * prop_threatening_hispanic
        )
    )
  )

district_borders_odd[[9]] <- NULL

district_borders <-
  district_borders_even %>%
  bind_rows() %>%
  bind_rows(
    district_borders_odd %>%
      bind_rows()
  ) %>%
  arrange(leaid, year)

rm(district_borders_even) 
rm(district_borders_odd)

rm(shapefiles)

save.image("threat_analysis_04_06_13.RData")

## 6) join district and threat data ----
d_dat <-
  d_dat %>% 
  left_join(district_borders)

## 7) Charter information ----

# number of charters per district per year
charter_dat <-
  s_dat %>%
  group_by(leaid, year) %>%
  summarize(
    n_charter = sum(charter, na.rm = T)
  ) %>%
  ungroup()

charter_demo <-
  s_dat %>% 
  filter(!is.na(charter)) %>% 
  group_by(leaid, year, charter) %>% 
  summarize(
    prop_white = sum(white, na.rm = T) / sum(total, na.rm = T),
    prop_black = sum(black, na.rm = T) / sum(total, na.rm = T),
    prop_asian = sum(asian, na.rm = T) / sum(total, na.rm = T),
    prop_hispanic = sum(hispanic, na.rm = T) / sum(total, na.rm = T),
    prop_other = sum(other, na.rm = T) / sum(total, na.rm = T),
    mean_sch_diversity = mean(school_diversity, na.rm = T)
  ) %>% 
  mutate_at(
    vars(starts_with("prop"), mean_sch_diversity),
    ~ round(., 2)
  ) %>% 
  mutate(
    charter = 
      ifelse(
        charter == 1, 
        "charter", 
        "traditional"
      )
  ) %>% 
  ungroup() %>%
  pivot_wider(names_from = charter, values_from = prop_white:mean_sch_diversity)

charter_dat <-
  charter_dat %>% 
  left_join(charter_demo)

# merge with district data and calculate proportion of charters
charter_dat <-
  charter_dat %>%
  left_join(
    d_dat
  ) %>% 
  mutate(
    prop_charter = n_charter / number_of_schools
  ) %>%
  group_by(leaid) %>%
  mutate(
    change_charter = n_charter - lag(n_charter)
  ) %>%
  ungroup()

# determine district-level segregation
tmp <-
  s_dat %>%
  filter(!is.na(white), !is.na(non_white)) %>% 
  group_by(year, leaid) %>%
  filter(white > 0, non_white > 0) %>% 
  group_nest()

tmp$data <-
  map(
    tmp$data,
    ~ mutate(.x, seg = .x %>% select(white, non_white) %>% seg::dissim(data = .) %>% purrr::pluck(1))
  )

tmp <- 
  tmp %>%
  unnest(cols = c(data)) %>%
  mutate(leaid = as.numeric(leaid)) %>%
  select(year, leaid, seg) %>%
  distinct()

charter_dat <-
  charter_dat %>%
  left_join(
    tmp
  ) %>% 
  # any years with 0 charters, charter pop is 0
  mutate_at(
    vars(matches("prop_[a-z]+_charter")),
    ~ ifelse(n_charter == 0, 0, .)
  )

rm(tmp)

# keep the districts in non-urban areas & majority white districts & affluent districts
charter_dat <- 
  charter_dat %>% 
  mutate(
    prop_local = rev_local_total / rev_total,
    prop_state = rev_state_total / rev_total,
    prop_local_prop_tax = rev_local_prop_tax / rev_total,
    per_pupil_spending = exp_total / total
  )

charter_dat_non_urban <-
  charter_dat %>%
  group_by(leaid) %>%
  filter(!urban_centric_locale %in% c(1, 2, 11, 12)) %>%
  ungroup()

charter_white_quant <-
  charter_dat %>% 
  mutate(
    white_prop = ifelse(white_prop == 0, NA, white_prop)
  ) %>% 
  group_by(leaid) %>% 
  fill(white, .direction = "up") %>% 
  summarize(
    mean_white = mean(white_prop, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    quant = ntile(mean_white, 5)
  )

charter_dat_maj_white <-
  charter_dat %>%
  filter(leaid %in% (charter_white_quant %>% filter(quant %in% 4:5) %>% pull(leaid)))

# some instances where white = 0 that appear to be errors; impute the previous value
charter_dat_maj_white <-
  charter_dat_maj_white %>% 
  mutate(
    white_prop = ifelse(white_prop == 0, NA, white_prop)
  ) %>% 
  group_by(leaid) %>% 
  fill(white, .direction = "up") %>% 
  ungroup()

charter_affluent <-
  charter_dat %>%
  group_by(year) %>%
  mutate(pov_25th = quantile(est_population_5_17_poverty_pct, 0.25, na.rm = T)) %>% 
  group_by(leaid) %>% 
  filter(sum(est_population_5_17_poverty_pct <= pov_25th, na.rm = T) > (n() / 2)) %>%
  ungroup()


summary(
  felm(
    prop_charter ~ 
      lag(prop_charter) +
      threat_overall + 
      seg + 
      prop_white_traditional +
      prop_black_traditional +
      prop_hispanic_traditional + 
      log(enrollment) +
      lag(prop_local) +
      lag(prop_local_prop_tax) + 
      lag(per_pupil_spending) | leaid + year | 0 | leaid,
    data = charter_dat_maj_white
  )
)


summary(
  felm(
    prop_charter ~ 
      lag(prop_charter) +
      threat_black + 
      threat_hispanic +
      seg + 
      prop_white_traditional +
      prop_black_traditional +
      prop_hispanic_traditional + 
      log(enrollment) +
      lag(prop_local) +
      lag(prop_local_prop_tax) + 
      lag(per_pupil_spending) | leaid + year | 0 | leaid,
    data = charter_dat_maj_white
  )
)

summary(
  felm(
    prop_white_charter ~ 
      threat_overall +
      seg + 
      prop_white_traditional +
      prop_black_traditional +
      prop_hispanic_traditional + 
      log(enrollment) +
      lag(prop_local) +
      lag(prop_local_prop_tax) + 
      lag(per_pupil_spending) | leaid + year | 0 | leaid,
    data = charter_dat_maj_white
  )
)

summary(
  felm(
    prop_white_charter ~ 
      threat_black + 
      threat_hispanic +
      seg + 
      prop_white_traditional +
      prop_black_traditional +
      prop_hispanic_traditional + 
      log(enrollment) +
      lag(prop_local) +
      lag(prop_local_prop_tax) + 
      lag(per_pupil_spending) | leaid + year | 0 | leaid,
    data = charter_dat_maj_white
  )
)


summary(
  felm(
    prop_black_charter ~ 
      threat_overall +
      seg + 
      prop_white_traditional +
      prop_black_traditional +
      prop_hispanic_traditional + 
      log(enrollment) +
      lag(prop_local) +
      lag(prop_local_prop_tax) + 
      lag(per_pupil_spending) | leaid + year | 0 | leaid,
    data = charter_dat_maj_white
  )
)

summary(
  felm(
    prop_hispanic_charter ~ 
      threat_overall +
      seg + 
      prop_white_traditional +
      prop_black_traditional +
      prop_hispanic_traditional + 
      log(enrollment) +
      lag(prop_local) +
      lag(prop_local_prop_tax) + 
      lag(per_pupil_spending) | leaid + year | 0 | leaid,
    data = charter_dat_maj_white
  )
)
