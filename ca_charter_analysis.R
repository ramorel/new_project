library(tidyverse)
library(educationdata)
library(tidycensus)
library(lfe)
library(sf)
library(huxtable)

load("charter_analysis.RData")
load("district_shapefiles.RData")

## California districts
d_dat <- 
  d_dat %>% 
  filter(fips == 6) %>% 
  select(-contains("est_population"))

## California schools
s_dat <-
  s_dat %>% 
  filter(fips == 6)

# CA poverty data
d_pov <-
  get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(
      year = 2000:2015,
      fips = 6
    )
  ) %>%
  mutate(leaid = as.numeric(leaid)) %>%
  as_tibble() %>%
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  arrange(leaid, year) %>% 
  select(leaid, year, contains("est_population"))

d_dat <- 
  left_join(d_dat, d_pov)

## Determine threat ----
shapefiles <- 
  map(shapefiles, ~ filter(.x, statefp == 6))

district_borders <-
  map(shapefiles, st_touches)

district_borders <-
  map2(
    district_borders,
    shapefiles,
    ~ map(
      .x,
      function(y)
        as.numeric(.y$geoid[y])
    )
  )

district_borders_odd <-
  map(
    seq(district_borders),
    ~ map_dfr(
      seq(district_borders[[.x]]),
      function(y)
        tibble(
          leaid = as.numeric(shapefiles[[.x]]$geoid[y]),
          year = shapefiles[[.x]]$year[y],
          prop_theatened_white =
            d_dat$white_prop[d_dat$leaid == as.numeric(shapefiles[[.x]]$geoid[y]) & d_dat$year == shapefiles[[.x]]$year[y]],
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

district_borders_even <-
  map(
    seq(district_borders),
    ~ map_dfr(
      seq(district_borders[[.x]]),
      function(y)
        tibble(
          leaid = as.numeric(shapefiles[[.x]]$geoid[y]),
          year = (shapefiles[[.x]]$year[y] + 1),
          prop_theatened_white =
            d_dat$white_prop[d_dat$leaid == as.numeric(shapefiles[[.x]]$geoid[y]) & d_dat$year == (shapefiles[[.x]]$year[y] + 1)],
          prop_threatening_overall =
            sum(
              c(
                d_dat$black[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == (shapefiles[[.x]]$year[y] + 1)],
                d_dat$hispanic[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == (shapefiles[[.x]]$year[y] + 1)]
              ),
              na.rm = T
            ) /
              sum(
                d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == (shapefiles[[.x]]$year[y] + 1)],
                na.rm = T
              ),
          prop_threatening_black =
            sum(
              c(
                d_dat$black[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == (shapefiles[[.x]]$year[y] + 1)]
              ),
              na.rm = T
            ) /
              sum(
                d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == (shapefiles[[.x]]$year[y] + 1)],
                na.rm = T
              ),
          prop_threatening_hispanic =
            sum(
              c(
                d_dat$hispanic[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == (shapefiles[[.x]]$year[y] + 1)]
              ),
              na.rm = T
            ) /
              sum(
                d_dat$total[d_dat$leaid %in% district_borders[[.x]][[y]] & d_dat$year == (shapefiles[[.x]]$year[y] + 1)],
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

district_borders_even[[9]] <- NULL

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

## 6) join district and threat data ----
d_dat <-
  d_dat %>%
  inner_join(district_borders)

## 7) Charter information ----
# number of charters per district per year
charter_dat <-
  s_dat %>%
  group_by(leaid, year) %>%
  summarize(
    n_charter = sum(charter, na.rm = T),
    n_total = n()
  ) %>%
  ungroup() %>% 
  mutate(prop_charter = n_charter / n_total) %>% 
  arrange(leaid, year)

charter_demo <-
  s_dat %>%
  group_by(leaid, year, charter) %>% 
  summarize_at(vars(hispanic:other), ~ sum(., na.rm = TRUE)) %>% 
  ungroup() %>%
  complete(charter, nesting(leaid, year), 
           fill = list(hispanic = 0,
                       white = 0, black = 0, 
                       native_am = 0, asian = 0, 
                       total = 0, pacific = 0, 
                       two_or_more = 0, other = 0)) %>% 
  arrange(leaid, year, charter) %>% 
  mutate(nonwhite = total - white) %>% 
  group_by(leaid, year) %>% 
  # share_within answers the question, "What percent of [race] students in this district are enrolled in {TPS,charter}?
  mutate_at(vars(hispanic:nonwhite), list(share_within = ~ round(./sum(., na.rm = TRUE), 2))) %>% 
  # share_across answers the question, "What percent of students in the district are [race] and enrolled in {TPS,charter}?
  mutate_at(vars(hispanic:nonwhite), list(share_across_district = ~ round(./sum(total, na.rm = TRUE), 2))) %>% 
  ungroup() %>%
  # share_btw answers the question, "What percent of {TPS,charter} students in this district are [race]?
  mutate_at(vars(hispanic:nonwhite), list(share_btw = ~ round(./total, 2))) %>% 
  mutate(
    charter =
      ifelse(
        charter == 1,
        "charter",
        "traditional"
      )
  ) %>%
  pivot_wider(names_from = charter, values_from = hispanic:last_col())

charter_dat <-
  charter_dat %>%
  left_join(charter_demo)

# merge with district data
charter_dat <-
  charter_dat %>%
  left_join(d_dat)

# Get overall share of each race/ethnicity in the district
charter_dat <-
  charter_dat %>% 
  mutate_at(vars(white:native_am, pacific, two_or_more, other), 
            list(overall_district_share = ~ . / total))

# keep the districts in non-urban areas & majority white districts & affluent districts
charter_dat <-
  charter_dat %>%
  mutate(
    prop_local_rev = rev_local_total / rev_total,
    prop_state_rev = rev_state_total / rev_total,
    prop_local_prop_tax = rev_local_prop_tax / rev_total,
    per_pupil_spending = exp_total / total
  )

charter_dat_non_urban <-
  charter_dat %>%
  group_by(leaid) %>%
  filter(!urban_centric_locale %in% c(1, 2, 11, 12)) %>%
  ungroup()

charter_dat_urban <-
  charter_dat %>%
  group_by(leaid) %>%
  filter(urban_centric_locale %in% c(1, 2, 11, 12)) %>%
  ungroup()

charter_maj_white <-
  charter_dat %>%
  group_by(leaid) %>% 
  mutate(maj_white = ifelse(white_overall_district_share >= 0.7, 1, 0)) %>% 
  filter(all(maj_white[year < 2005] == 1)) %>% 
  ungroup()

# Affluent -- either est% children in poverty or 2010 acs median income?
charter_affluent <-
  charter_dat %>%
  group_by(leaid) %>%
  mutate(affluent = ifelse(est_population_5_17_poverty_pct < 0.1, 1, 0)) %>% 
  filter(all(affluent[year < 2005] == 1)) %>% 
  ungroup()

median_income <- 
  map(c("school district (elementary)", "school district (unified)", "school district (secondary)"),
      ~ get_acs(geography = .x, variables = "B19013_001", year = 2010, state = "CA", geometry = FALSE)) %>% 
  bind_rows() %>% 
  select(GEOID, estimate) %>% 
  mutate(GEOID = as.numeric(GEOID))

charter_affluent2 <- 
  charter_dat %>% 
  left_join(median_income, by = c("leaid" = "GEOID")) %>% 
  filter(estimate >= quantile(estimate, 0.6, na.rm = TRUE))

rm(list = ls()[str_detect(ls(), "d_")][-2])
rm(list = ls()[str_detect(ls(), "s_")])

save.image("ca_threat_analysis.RData")


# Models 1: Share of charters in a district ----
m1_base <- 
  felm(
    prop_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional | 0 | 0 | leaid,
    data = charter_dat
  )

m1_base_fes <- 
  felm(
    prop_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional | leaid + year | 0 | leaid,
    data = charter_dat
  )

m1_pooled <- 
  felm(
    prop_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional +
      white_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | 0 | 0 | leaid,
    data = charter_dat
  )

m1_controls <- 
  felm(
    prop_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional +
      white_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat
  )

coefs_names <- c("Proximal Threat", "%Black in TPS", "%Hispanic in TPS")
huxreg(list(m1_base, m1_base_fes ,m1_pooled, m1_controls), 
       coefs = c("Proximal threat" = "threat_overall",
                 "%Black in TPS" = "black_share_btw_traditional", 
                 "%Hispanic in TPS" = "hispanic_share_btw_traditional"),
       statistics = c('N' = 'nobs', 'R-squared' = 'r.squared'),
       note = "{stars}. \nStandard errors clustered at the district level. \nControls include the natural log of enrollment, local revenue, and per-student spending, and the share of 5-17 in poverty.") %>% 
  insert_row(c("District Fixed Effects", "", "X", "", "X"), after = 7) %>% 
  insert_row(c("Year Fixed Effects", "", "X", "", "X"), after = 8) %>% 
  insert_row(c("Controls", "", "", "X", "X"), after = 9) %>% 
  set_bottom_border(8, 2:5, 0) %>% 
  set_bottom_border(9, 2:5, 0) %>% 
  set_bottom_border(10, 2:5, 0)
  

# Models 2: White charter enrollment in a district ----
m2_base <- 
  felm(
    white_share_within_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional | 0 | 0 | leaid,
    data = charter_dat
  )

m2_base_fes <- 
  felm(
    white_share_within_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional | leaid + year | 0 | leaid,
    data = charter_dat
  )

m2_pooled <- 
  felm(
    white_share_within_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | 0 | 0 | leaid,
    data = charter_dat
  )

m2_controls <- 
  felm(
    white_share_within_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat
  )

huxreg(list("pooled" = m2_base, "fes" = m2_base_fes, "pooled" = m2_pooled, "fes" = m2_controls))

# Models 3: Proximal threat ----
m3_overall_prox <- 
  felm(
    white_share_within_charter ~
      threat_overall +
      white_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat
  )

m3_prox <- 
  felm(
    white_share_within_charter ~
      threat_black +
      threat_hispanic + 
      white_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat
  )

huxreg(list(m3_overall_prox, m3_prox))

# Models 4: Direct threat ----
m4_overall_direct <- 
  felm(
    white_share_within_charter ~
      nonwhite_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat
  )

m4_direct <- 
  felm(
    white_share_within_charter ~
      black_share_btw_traditional +
      hispanic_share_btw_traditional +
      white_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat
  )

huxreg(list(m4_overall_direct, m4_direct))

# Models 5: Urban v Non-urban ----
m5_non <- 
  felm(
    white_share_within_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional +
      white_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat_non_urban
  )

m5_urban <- 
  felm(
    white_share_within_charter ~
      threat_overall +
      black_share_btw_traditional +
      hispanic_share_btw_traditional +
      white_share_btw_traditional +
      log(enrollment) +
      log(rev_local_total) +
      log(per_pupil_spending) +
      est_population_5_17_poverty_pct | leaid + year | 0 | leaid,
    data = charter_dat_urban
  )

huxreg(list(m5_urban, m5_non))


