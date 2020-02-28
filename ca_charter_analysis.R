library(tidyverse)
library(educationdata)
library(tidycensus)
library(lfe)
library(sf)

load("charter_analysis.RData")
load("district_shapefiles.RData")

## California districts
d_dat <- 
  d_dat %>% 
  filter(fips == 6)

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

rm(shapefiles)

save.image("threat_analysis_04_06_08_26_41.RData")

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
    n_charter = sum(charter, na.rm = T)
  ) %>%
  ungroup() %>%
  arrange(leaid, year)

charter_demo <-
  s_dat %>%
  group_by(leaid, year, charter) %>%
  summarize(
    prop_white = sum(white, na.rm = T) / sum(total, na.rm = T),
    prop_black = sum(black, na.rm = T) / sum(total, na.rm = T),
    prop_asian = sum(asian, na.rm = T) / sum(total, na.rm = T),
    prop_hispanic = sum(hispanic, na.rm = T) / sum(total, na.rm = T),
    prop_other = sum(other, na.rm = T) / sum(total, na.rm = T)
  ) %>%
  ungroup() %>% 
  complete(charter, nesting(leaid, year), 
           fill = list(prop_white = 0, prop_black = 0, 
                       prop_asian = 0, prop_hispanic = 0, 
                       prop_other = 0)) %>% 
  arrange(leaid, year, charter) %>% 
  mutate_at(
    vars(starts_with("prop")),
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
  pivot_wider(names_from = charter, values_from = prop_white:last_col())

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

charter_dat <-
  charter_dat %>%
  filter(number_of_schools != 1)

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
  group_by(leaid, fips) %>%
  fill(white, .direction = "up") %>%
  summarize(
    mean_white = mean(white_prop, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(fips) %>%
  mutate(
    quant = ntile(mean_white, 5)
  ) %>%
  ungroup()

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
      threat_overall +
      prop_white_traditional +
      prop_black_traditional +
      prop_hispanic_traditional +
      log(enrollment) +
      lag(prop_local) +
      lag(prop_local_prop_tax) +
      lag(per_pupil_spending) | leaid + year | 0 | leaid,
    data = charter_dat
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


summary(
  felm(
    prop_black_charter ~
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
    prop_hispanic_charter ~
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