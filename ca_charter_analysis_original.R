library(tidyverse)
library(educationdata)
library(rvest)
library(tidycensus)
library(lfe)
library(sf)

######################
## 1) Get the data! ##
######################

## 1) School-level data ----

sdir <-
  get_education_data(
    level = "schools",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 1998:2016,
      fips = 6
    )
  ) %>%
  mutate(
    leaid = as.numeric(leaid)
  )

senroll <-
  map(
    1998:2016,
    ~ get_education_data(
      level = "schools",
      source = "ccd",
      topic = "enrollment",
      filters = list(
        year = .x,
        grade = 99,
        fips = 6
      ),
      by = list("race")
    ) 
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
                  `99` = "total")
    ) %>%
  reshape2::dcast(ncessch_num + year + leaid ~ race, value.var = "enrollment")

ca_dat <-
  left_join(
    sdir,
    senroll,
    by = c("ncessch_num", "year")
  ) %>%
  left_join(
    sassess
  ) %>% 
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  filter(total != 0) %>%
  as_tibble()

ca_dat <-
  ca_dat %>%
  group_by(ncessch_num, year) %>% 
  mutate(other = sum(native_am, pacific, two_or_more, unknown, na.rm = T)) %>% 
  mutate_at(
    vars(asian:hispanic, white, other),
    list(prop = ~ . / total)
  ) %>% 
  ungroup() 

ca_dat$non_white <- 
  ca_dat %>%
  select(asian, black, hispanic, other) %>% rowSums(na.rm = T)

ca_dat$school_diversity <-
  ca_dat %>% 
  select(white, non_white) %>% 
  as.matrix() %>%
  vegan::diversity(index = "simpson")

## 2) District data ----
d_dir <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(
      year = 1998:2016,
      fips = 6
    )
  ) %>%
  mutate(
    leaid = as.numeric(leaid)
  )

d_enroll <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    filters =
      list(
        year = 1998:2016,
        grade = 99,
        fips = 6
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
  select(year, leaid, race, enrollment) %>% 
  pivot_wider(names_from = race, values_from = enrollment)

#d_assess <- 
#  get_education_data(
#    level = "school-districts",
#    source = "edfacts",
#    topic = "assessments",
#    filters = list(year = 2009:2016, 
#                   grade_edfacts = 99))


d_assess <-
  haven::read_stata("/Users/rap168/Downloads/seda_geodist_long_cs_v30.dta") %>% 
  filter(fips == "06") %>% 
  mutate(leaid = as.numeric(leaidC)) %>% 
  arrange(leaid, year, subject, grade) %>% 
  group_by(leaid, year, subject) %>% 
  summarize(
    mean_mn_all = mean(mn_all, na.rm = T), 
    mean_mn_hsp = mean(mn_hsp, na.rm = T), 
    mean_mn_wht = mean(mn_wht, na.rm = T), 
    mean_mn_blk = mean(mn_blk, na.rm = T), 
    mean_mn_wbg = mean(mn_wbg, na.rm = T), 
    mean_mn_whg = mean(mn_whg, na.rm = T)
    ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = subject, values_from = c(mean_mn_all:mean_mn_whg)) %>% 
  mutate(year = as.numeric(year - 1))

d_dat <-
  left_join(d_dir, d_enroll) %>%
  left_join(d_assess) %>% 
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9) %>%
  filter(total != 0) %>%
  as_tibble() %>% 
  group_by(year, leaid) %>% 
  mutate(other = sum(native_am, pacific, two_or_more, unknown, na.rm = T)) %>% 
  mutate_at(
    vars(white, black, hispanic, asian, other),
    list(prop = ~ . / total)
  ) %>% 
  ungroup()

d_dat$district_diversity <-
  d_dat %>%
  select(asian, black, hispanic, white, other) %>%
  mutate(non_white = rowSums(.[, -4], na.rm = T)) %>% 
  select(contains("white")) %>% 
  as.matrix() %>%
  vegan::diversity(index = "simpson")

## 3) District poverty ----
d_pov <-
  get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(
      year = 1999:2016,
      fips = 6
    )
  ) %>% 
  mutate(leaid = as.numeric(leaid)) %>%
  as_tibble() %>% 
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9)

## 4) Finance data

d_fin <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "finance",
    filters = 
      list(
        year = 1998:2015,
        fips = 6
        )
    ) %>% 
  mutate(leaid = as.numeric(leaid)) %>% 
  as_tibble() %>% 
  na_if(-1) %>%
  na_if(-2) %>%
  na_if(-3) %>%
  na_if(-9)

## 5) threat data ----
load("ca_shapefiles.RData")

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
%>% 
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


## 6) join district and threat data ----
d_dat <-
  d_dat %>% 
  left_join(
    d_pov %>% 
      select(-fips)
  ) %>% 
  left_join(
    d_fin %>% 
      select(-fips)
  ) %>%
  left_join(district_borders)

## 7) Charter information ----

# number of charters per district per year
ca_charter <-
  ca_dat %>%
  group_by(leaid, year) %>%
  summarize(
    n_charter = sum(charter, na.rm = T)
    ) %>%
  ungroup()

ca_charter_demo <-
  ca_dat %>% 
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

ca_charter <-
  ca_charter %>% 
  left_join(ca_charter_demo)

# merge with district data and calculate proportion of charters
ca_charter <-
  ca_charter %>%
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
  ca_dat %>%
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

ca_charter <-
  ca_charter %>%
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
ca_charter <- 
  ca_charter %>% 
  mutate(
    prop_local = rev_local_total / rev_total,
    prop_state = rev_state_total / rev_total,
    prop_local_prop_tax = rev_local_prop_tax / rev_total,
    per_pupil_spending = exp_total / total
  )

ca_charter_non_urban <-
  ca_charter %>%
  group_by(leaid) %>%
  filter(!urban_centric_locale %in% c(1, 2, 11, 12)) %>%
  ungroup()

ca_white_quant <-
  ca_charter %>% 
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

ca_charter_maj_white <-
  ca_charter %>%
  filter(leaid %in% (ca_white_quant %>% filter(quant %in% 4:5) %>% pull(leaid)))

# some instances where white = 0 that appear to be errors; impute the previous value
ca_charter_maj_white <-
  ca_charter_maj_white %>% 
  mutate(
    white_prop = ifelse(white_prop == 0, NA, white_prop)
  ) %>% 
  group_by(leaid) %>% 
  fill(white, .direction = "up") %>% 
  ungroup()

ca_charter_affluent <-
  ca_charter %>%
  group_by(year) %>%
  mutate(pov_25th = quantile(est_population_5_17_poverty_pct, 0.25, na.rm = T)) %>% 
  group_by(leaid) %>% 
  filter(sum(est_population_5_17_poverty_pct <= pov_25th, na.rm = T) > (n() / 2)) %>%
  ungroup()


## Visualizations ----
ca_charter %>% 
  group_by(year) %>% 
  summarize(
    charter = sum(n_charter, na.rm = T),
    trad = sum(number_of_schools, na.rm = T)
    ) %>% 
  ungroup() %>% 
  pivot_longer(-year, names_to = "type", values_to = "count") %>% 
  ggplot(aes(group = type)) +
  geom_line(aes(x = factor(year), y = count, color = type)) +
  scale_color_brewer(palette = "Set3") +
  theme_minimal()

ca_charter %>% 
  group_by(year) %>% 
  summarize(
    charter = sum(n_charter, na.rm = T),
    trad = sum(number_of_schools, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(prop = charter / trad) %>% 
  ggplot(aes(group = 1)) +
  geom_line(aes(x = factor(year), y = prop)) +
  scale_color_brewer(palette = "Set3") +
  theme_minimal()


ca_charter %>%
  group_by(year) %>% 
  mutate(
    quant_white = forcats::as_factor(ntile(white_prop, 5))
  ) %>% 
  filter(!is.na(quant_white)) %>% 
  group_by(year, quant_white) %>%
  summarize(
    charter = sum(n_charter, na.rm = T)
  ) %>% 
  ungroup()  %>% 
  pivot_longer(cols = charter, names_to = "type", values_to = "count") %>% 
  ggplot(aes(x = factor(year), y = count, group = quant_white)) +
  geom_line(aes(color = quant_white)) +
  scale_color_viridis_d() +
  theme_minimal()

ca_charter %>%
  group_by(year) %>% 
  mutate(
    quant_white = forcats::as_factor(ntile(white_prop, 5))
  ) %>% 
  filter(!is.na(quant_white)) %>% 
  group_by(year, quant_white) %>%
  summarize(
    charter = sum(n_charter, na.rm = T),
    trad = sum(number_of_schools, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(prop = charter / trad) %>% 
  ggplot(aes(x = factor(year), y = prop, group = quant_white)) +
  geom_line(aes(color = quant_white)) +
  scale_color_viridis_d() +
  theme_minimal()

ca_charter %>%
  group_by(year) %>% 
  mutate(
    quant_white = forcats::as_factor(ntile(white_prop, 5)),
    quant_seg = forcats::as_factor(ntile(seg, 5))
  ) %>% 
  filter(
    !is.na(quant_white),
    !is.na(quant_seg)
    ) %>% 
  group_by(year, quant_white, quant_seg) %>%
  summarize(
    charter = sum(n_charter, na.rm = T),
    trad = sum(number_of_schools, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(prop = charter / trad) %>% 
  ggplot(aes(x = factor(year), y = prop, group = quant_white)) +
  geom_line(aes(color = quant_white)) +
  scale_color_viridis_d() +
  theme_minimal() +
  facet_wrap(~ quant_seg)

ca_charter %>%
  group_by(year) %>% 
  mutate(
    quant_white = forcats::as_factor(ntile(white_prop, 5)),
    quant_seg = forcats::as_factor(ntile(seg, 5))
  ) %>% 
  filter(
    !is.na(quant_white),
    !is.na(quant_seg)
  ) %>% 
  group_by(year, quant_white, quant_seg) %>%
  summarize(
    charter = sum(n_charter, na.rm = T),
    trad = sum(number_of_schools, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(prop = charter / trad) %>% 
  ggplot(aes(x = factor(year), y = prop, group = quant_seg)) +
  geom_line(aes(color = quant_seg)) +
  scale_color_viridis_d() +
  theme_minimal() +
  facet_wrap(~ quant_white)


## Maps
shapefiles[10:18] <-
  shapefiles %>% 
  map(~ mutate(.x, year = year + 1))

shapefiles[[18]] <- NULL

geo <- 
  map(shapefiles, ~ select(.x, year, contains("GEOID"), geometry)) %>% 
  map(~ rename_at(.x, vars(contains("10")), ~ str_remove(., "10"))) %>% 
  reduce(rbind)

ca_geo_charter <-
  ca_charter %>% 
  left_join(
    shapefiles[[1]] %>% select(-year) %>% 
      mutate(GEOID = as.numeric(GEOID)),
    by = c("leaid" = "GEOID"))

ca_geo_charter %>% 
  filter(year > 2000) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop_charter)) + 
  scale_fill_viridis_c() + 
  coord_sf() + 
  theme_void() +
  facet_wrap(~ year)
  

## Census data
geos <- c(
  "school district (unified)",
  "school district (elementary)",
  "school district (secondary)"
)
vars <- c(
  tot_pop = "B01001_001",
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
  total_male_bach = "B15002_015",
  total_female_bach = "B15002_032",
  med_income = "B19013_001"
)

acs_dat <- list()
years <- c(2010:2017)

for (i in seq(years)) {
  acs_dat[[i]] <-
    map(
      geos[1:2],
      ~ get_acs(
        geography = .x,
        variables = vars,
        output = "wide",
        year = years[i],
        state = 6
      )
    ) %>%
    bind_rows() %>%
    mutate(
      year = years[i]
    )
}

acs_dat <-
  bind_rows(acs_dat)

options(tigris_use_cache = TRUE)
county_acs <-
  map(
    years,
    ~ get_acs(
      geography = "county",
      variables = vars,
      output = "wide",
      year = .x,
      state = 6,
      geometry = TRUE
    ) %>%
      mutate(
        year = .x
      )
  ) %>%
  bind_rows()



d_dat <-
  left_join(
    d_dat,
    acs_dat %>%
      mutate(leaid = as.numeric(GEOID))
  ) %>%
  mutate(
    county_code = as.numeric(county_code)
  ) %>%
  left_join(
    county_acs %>%
      mutate(GEOID = as.numeric(GEOID)),
    by = c("county_code" = "GEOID", "year")
  )




# data frame of charter ids and longitude/latitude
charter_lon_lat <-
  ca_dat %>%
  filter(charter == 1) %>%
  select(year, school_id, leaid, longitude, latitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

coordinates(charter_lon_lat) <- ~ longitude + latitude

charter_lon_lat <-
  charter_lon_lat %>%
  st_as_sf() %>%
  st_sf(crs = 4326) %>%
  lwgeom::st_make_valid()


# Join charter schools and district data
chart_count <-
  ca_dat %>%
  filter(charter == 1) %>%
  group_by(year, leaid) %>%
  summarize(n_charters = n()) %>%
  ungroup() %>%
  arrange(leaid, year) %>%
  complete(year, nesting(leaid), fill = list(n_charters = 0)) %>%
  arrange(leaid, year)

ca_dat %>%
  filter(charter == 1) %>%
  mutate(
    white_cate =
      case_when(
        white_prop < 0.1 ~ "exclusively non-white",
        white_prop >= 0.1 & white_prop < 0.5 ~ "minority white",
        white_prop >= 0.5 & white_prop < 0.9 ~ "majority white",
        white_prop >= 0.9 ~ "exclusively white",
        TRUE ~ NA_character_
      )
  ) %>%
  group_by(year, white_cate, .drop = T) %>%
  summarize(n = n()) %>%
  View()


chart_demo <-
  ca_dat %>%
  filter(charter == 1) %>%
  mutate()
group_by(year, leaid) %>%
  summarise_at(vars(free_lunch:free_or_reduced_price_lunch, asian:diversity), sum, na.rm = T) %>%
  ungroup() %>%
  mutate_at(vars(-year, -leaid, -total), list(charter = ~ . / total)) %>%
  select(year, leaid, ends_with("_charter"))

n_schools <-
  ca_dat %>%
  group_by(leaid, year) %>%
  summarize(n_schools = n()) %>%
  ungroup() %>%
  arrange(leaid, year)

chart_count <-
  chart_count %>%
  left_join(n_schools)

chart_count <-
  chart_count %>%
  group_by(leaid) %>%
  mutate(change = n_charters - lag(n_charters)) %>%
  ungroup() %>%
  mutate(
    prop_charter = n_charters / n_schools
  )

chart_count <-
  chart_count %>%
  mutate(leaid = as.numeric(leaid)) %>%
  left_join(d_dat) %>%
  mutate_at(
    vars(asian:white),
    list(prop = ~ . / total)
  )


chart_count <-
  chart_count %>%
  mutate(maj_white_asian = ifelse(rowSums(chart_count %>% select(black_prop, hispanic_prop), na.rm = T) < 0.2, 1, 0))


chart_count <-
  chart_count %>%
  left_join(
    tmp %>% select(year, leaid, mean_seg)
  )


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
    data = ca_charter_maj_white
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
    data = ca_charter_maj_white
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
      lag(per_pupil_spending)| leaid + year | 0 | leaid,
    data = ca_charter_maj_white
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
      prop_local +
      prop_local_prop_tax + 
      per_pupil_spending | leaid + year | 0 | leaid,
    data = ca_charter_maj_white
  )
)
