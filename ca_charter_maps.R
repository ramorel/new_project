library(tidyverse)
library(gganimate)
library(sf)
library(tigris)

load("ca_charter_analysis.RData")
options(tigris_use_cache = TRUE, class = "sf")

## charter density
ca_charter <- 
  ca_charter %>% 
  mutate(charter_dens = (n_charter / total) * 1000) %>% 
  group_by(leaid) %>% 
  mutate(urban_centric_locale = max(urban_centric_locale, na.rm = TRUE)) %>% 
  filter(!is.infinite(urban_centric_locale)) %>% 
  mutate(
    max_charter = log(max(n_charter) + 1), 
    max_prop_charter = max(prop_charter),
    max_charter_dens = max(charter_dens),
    locale = 
      case_when(
        urban_centric_locale %in% c(1:2, 11:13) ~ "urban",
        urban_centric_locale %in% c(3:4, 21:23) ~ "suburban",
        urban_centric_locale %in% c(5:6, 31:33) ~ "town",
        urban_centric_locale %in% c(7:8, 41:43) ~ "rural",
        TRUE ~ NA_character_
      ),
    major_city = 
      case_when(
        urban_centric_locale %in% c(1, 11) ~ "major city",
        urban_centric_locale %in% c(2:6, 12:33) ~ "suburb, small city, or town",
        urban_centric_locale %in% c(7:8, 41:43) ~ "rural",
        TRUE ~ NA_character_
      )
  ) %>% 
  ungroup()

zoom_la <- c(-118.15, 34.03)

zoom_level <- 7

lon_span <- 360 / 2^zoom_level
lat_span <- 180 / 2^zoom_level

lon_bounds <- c(zoom_la[1] - lon_span / 2, zoom_la[1] + lon_span / 2)
lat_bounds <- c(zoom_la[2] - lat_span / 2, zoom_la[2] + lat_span / 2)

ggplot() + 
  geom_sf(data = shapefiles[[1]], aes(geometry = geometry)) + 
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) +
  theme_bw()


shp1 <- 
  st_crop(
  shapefiles[[5]], 
  xmin = min(lon_bounds), xmax = max(lon_bounds),
  ymin = min(lat_bounds), ymax = max(lat_bounds)
  )

tmp <- left_join(shp1 %>% mutate(GEOID = as.numeric(GEOID)), ca_charter, by = c("GEOID" = "leaid"))

tmp %>% 
  group_by(GEOID) %>% 
  mutate(urban_centric_locale = max(urban_centric_locale, na.rm = TRUE)) %>% 
  filter(!is.infinite(urban_centric_locale)) %>% 
  mutate(
    max_charter = log(max(n_charter) + 1), 
    locale = 
      case_when(
        urban_centric_locale %in% c(1:2, 11:13) ~ "urban",
        urban_centric_locale %in% c(3:4, 21:23) ~ "suburban",
        urban_centric_locale %in% c(5:6, 31:33) ~ "town",
        urban_centric_locale %in% c(7:8, 41:43) ~ "rural",
        TRUE ~ NA_character_
      ),
    major_city = 
      case_when(
        urban_centric_locale %in% c(1, 11) ~ "major city",
        urban_centric_locale %in% c(2:6, 12:33) ~ "suburb, small city, or town",
        urban_centric_locale %in% c(7:8, 41:43) ~ "rural",
        TRUE ~ NA_character_
      )
    ) %>% 
  filter(!is.na(locale)) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = max_charter)) +
  geom_sf(data = tmp %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent") +
  scale_fill_distiller(direction = 1) +
  scale_color_brewer(palette = "Dark2") + 
  theme_void()

tmp %>% 
  group_by(GEOID) %>% 
  mutate(urban_centric_locale = max(urban_centric_locale, na.rm = TRUE)) %>% 
  filter(!is.infinite(urban_centric_locale)) %>% 
  mutate(
    max_prop_charter = max(prop_charter), 
    locale = 
      case_when(
        urban_centric_locale %in% c(1:2, 11:13) ~ "urban",
        urban_centric_locale %in% c(3:4, 21:23) ~ "suburban",
        urban_centric_locale %in% c(5:6, 31:33) ~ "town",
        urban_centric_locale %in% c(7:8, 41:43) ~ "rural",
        TRUE ~ NA_character_
      ),
    major_city = 
      case_when(
        urban_centric_locale %in% c(1, 11) ~ "major city",
        urban_centric_locale %in% c(2:6, 12:33) ~ "suburb, small city, or town",
        urban_centric_locale %in% c(7:8, 41:43) ~ "rural",
        TRUE ~ NA_character_
      )
  ) %>% 
  filter(!is.na(locale)) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = max_prop_charter)) +
  geom_sf(data = tmp %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent") +
  scale_fill_distiller(direction = 1) +
  theme_void()


p <- 
  left_join(
    st_crop(
      shapefiles[[5]], 
      xmin = min(lon_bounds), xmax = max(lon_bounds),
      ymin = min(lat_bounds), ymax = max(lat_bounds)
    ) %>% 
      mutate(GEOID = as.numeric(GEOID)),
  ca_charter %>% 
    group_by(leaid) %>% 
    summarize(
      max_n = max(n_charter, na.rm = TRUE),
      map_prop = max(prop_charter, na.rm = TRUE)
      ) %>% 
    ungroup() %>% 
    mutate(
      max_n = max_n / sum(max_n, na.rm = T)
    ) %>% 
    pivot_longer(cols = -leaid),
  by = c("GEOID" = "leaid")
  ) %>% 
  filter(!is.na(name)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = value)) +
  scale_fill_distiller(direction = 1) +
  theme_void() +
  transition_manual(name) 

animate(p, duration = 3)



map_dat <- 
  map(shapefiles, ~ left_join(.x %>% mutate(GEOID = as.numeric(GEOID)), ca_charter, by = c("GEOID" = "leaid", "year"))) 

vars_to_keep <- reduce(map(map_dat, names), intersect)

map_dat <- 
  map_dat %>% 
  map(~ select(.x, one_of(vars_to_keep))) %>% 
  reduce(rbind) %>% 
  mutate(log_charter = log(n_charter + 1)) 
    
map_dat <- 
  right_join(shapefiles[[6]] %>% mutate(GEOID = as.numeric(GEOID)) %>% select(-year), ca_charter, by = c("GEOID" = "leaid")) %>% 
  mutate(log_charter = log(n_charter + 1)) 

p1 <- 
  map_dat %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = log_charter), size = 0.1) +
  geom_sf(data = map_dat %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent", size = 0.5) +
  scale_fill_distiller(direction = 1) +
  labs(title = "Number of Charter Schools", subtitle = "Year: {current_frame}") +
  theme_void() +
  transition_manual(year) 

animate(p1, fps = 5)

anim_save("ca_n_charter.gif")


p2 <- 
  map_dat %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = prop_charter), size = 0.1) +
  geom_sf(data = map_dat %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent", size = 0.5) +
  scale_fill_distiller(direction = 1) +
  labs(title = "Share of Charter Schools", subtitle = "Year: {current_frame}") +
  theme_void() +
  transition_manual(year) 

animate(p2, fps = 5)

anim_save("ca_prop_charter.gif")


p3 <- 
  map_dat %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = charter_devns), size = 0.1) +
  geom_sf(data = map_dat %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent", size = 0.5) +
  scale_fill_distiller(direction = 1) +
  labs(title = "Charter Schools per 1000 students", subtitle = "Year: {current_frame}") +
  theme_void() +
  transition_manual(year) 

animate(p3, fps = 5)

anim_save("ca_dens_charter.gif")


tmp <- left_join(shapefiles[[6]] %>% mutate(GEOID = as.numeric(GEOID)), ca_charter, by = c("GEOID" = "leaid")) 

p1 <- 
  tmp %>% 
  filter(!is.na(locale)) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = max_charter), size = 0.1) +
  geom_sf(data = tmp %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent") +
  scale_fill_distiller(direction = 1) +
  theme_void()

ggsave("ca_charter_n.png", dpi = 300)

p2 <- 
  tmp %>% 
  filter(!is.na(locale)) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = max_prop_charter), size = 0.1) +
  geom_sf(data = tmp %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent") +
  scale_fill_distiller(direction = 1) +
  theme_void()

ggsave("ca_charter_prop.png", dpi = 300)

p3 <- 
  tmp %>% 
  filter(max_charter_dens < max(max_charter_dens, na.rm = T)) %>% 
  mutate(max_charter_dens = log(max_charter_dens + 1)) %>% 
  filter(!is.na(locale)) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = max_charter_dens), size = 0.1) +
  geom_sf(data = tmp %>% filter(urban_centric_locale %in% c(1, 11)), color = "red", fill = "transparent") +
  scale_fill_distiller(direction = 1) +
  theme_void()

ggsave("ca_charter_dens.png", dpi = 300)



coast <- st_read("/Users/rap168/Downloads/tl_2017_us_coastline/tl_2017_us_coastline.shp") %>% st_as_sf() %>% filter(NAME == "Pacific") %>% st_transform(4326)
ca_coast <- st_crop(coast, xmin = -124.482, ymin = 32.52883, xmax = -114.1312, ymax = 42.00952)
ca_coast <- st_union(st_combine(ca_coast))

st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

ca_no_coast <- 
  st_difference(shapefiles[[6]], ca_coast)


ca_water <- map(list_counties("CA")$county, ~ area_water("CA", .x, class = "sf"))
ca_water <- map(ca_water, st_transform, 4326)
ca_water <- map(ca_water, ~ .x[str_detect(.x$FULLNAME, "Bay"), ])
ca_water <- map(ca_water, ~ .x[!is.na(.x$FULLNAME), ])
ca_water <- ca_water[which(map_dbl(ca_water, nrow) != 0)]
ca_water <- reduce(ca_water, rbind)
ca_water <- st_union(st_combine(ca_water))


ca_no_coast <- shapefiles[[6]]
  
for (i in ca_water) {
  ca_no_coast <- st_difference(ca_no_coast, i)
  }
  


bivariate_color_scale <- tibble(
  group = c("3 - 3",
            "2 - 3",
            "1 - 3",
            "3 - 2",
            "2 - 2",
            "1 - 2",
            "3 - 1",
            "2 - 1",
            "1 - 1"), fill = 
    c("#3B4994",
      "#8C62AA",
      "#BE64AC",
      "#5698B9",
      "#A5ADD3",
      "#DFB0D6",
      "#5AC8C8",
      "#ACE4E4",
      "#E8E8E8")
)

tmp <- left_join(shapefiles[[6]] %>% mutate(GEOID = as.numeric(GEOID)), ca_charter, by = c("GEOID" = "leaid"))

# create 3 buckets for gini
quantiles_prop <- tmp %>%
  pull(max_prop_charter) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

# create 3 buckets for mean income
quantiles_white <- tmp %>%
  pull(white_prop) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

tmp %<>%
  mutate(
    prop_quantiles = cut(
      prop_charter,
      breaks = c(0, 0.01, 0.0833, 1),
      include.lowest = TRUE
    ),
    white_quantiles = cut(
      white_prop,
      breaks = quantiles_white,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(prop_quantiles), "-",
      as.numeric(white_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")


bivariate_color_scale <- 
  bivariate_color_scale %>% 
  separate(group, into = c("white", "prop_charter"), sep = " - ") %>%
  mutate(white = as.integer(white),
         prop_charter = as.integer(prop_charter))

p <- ggplot(tmp) +
  geom_sf(aes(fill = fill),
          color = "white",
          size = 0.1) +
  scale_fill_identity() +
  theme_void()

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = gini,
      y = mean,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher inequality ⟶️",
       y = "Higher income ⟶️") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()

ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = white,
      y = prop_charter,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher inequality ⟶️",
       y = "Higher income ⟶️") + 
  theme_bw()
