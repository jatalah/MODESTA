rm(list = ls())
library(tidyverse)
library(sf)
library(stars)
library(mapview)
sf_use_s2(FALSE)

farm_centroids_buffer <- 
  read_csv('data/farm_centroids.csv') %>% 
  st_as_sf(coords = c("X", "Y"),
           crs = st_crs(4326)) %>% 
  st_buffer(0.02)
  
# get max depth for farms----------
max_farm_depth <- 
  read_stars('data/gebco_2022_n42.4292_s34.4751_w-6.6577_e3.0981.tif', crs = st_crs(4326), make_units = F) %>% 
  .[.<0] %>% 
  rename(depth = 1) %>% 
  st_crop(farm_centroids_buffer) %>% 
  as_tibble() %>% 
  mutate(depth = -as.numeric(depth)) %>% 
  # filter(depth>=0) %>% 
  drop_na() %>% 
  st_as_sf(coords = c("x", "y"),
           crs = st_crs(4326)) %>% 
  st_join(farm_centroids_buffer, join = st_nearest_feature, left = T) %>%
  mutate(
    longitude = sf::st_coordinates(.)[, 1],
    latitude = sf::st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(name) %>% 
  summarise(max_depth = max(depth),
            mean_depth = mean(depth))

max_farm_depth

write_csv(mean_farm_depth, 'data/farm_depth.csv')
