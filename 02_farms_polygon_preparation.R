rm(list = ls())
library(sf)
sf_use_s2(FALSE)

recintos <- 
  read_sf('C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/acuivisor_data/Recintos.shp') %>% 
  st_crop(st_bbox(c(xmin = -1.9, ymin = 37.374720,  xmax = 1.11, ymax = 38.73))) %>% 
  mutate(area = as.numeric(st_area(.)))

costa <- 
  read_sf('C:/Users/javiera/OneDrive - Cawthron/UA/copernicus_training/data/costa_blanca.geojson') %>% 
  st_crop(st_bbox(c(xmin = -1.9, ymin = 37.374720,  xmax = 1.11, ymax = 38.73)))

# create buffer ----
recintos_buffer <-
  recintos %>%
  st_buffer(dist = 0.04) %>%
  st_difference(st_buffer(costa, dist = 0.006)) %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(ID = 1:nrow(.), name = rev(
    c(
      "Calp y Altea",
      "La Vila Joiosa",
      "El Campello",
      "Guardamar",
      "San Pedro",
      "EL Gorguel",
      "Aguilas"
    )
  ),
  farm_code = LETTERS[1:7] %>% rev()) %>%
  mutate(area_km2 = round(as.numeric(st_area(.) / 1e6), 3))


farm_centroids <-
  recintos_buffer %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(name = rev(
    c(
      "Calp y Altea",
      "La Vila Joiosa",
      "El Campello",
      "Guardamar",
      "San Pedro",
      "EL Gorguel",
      "Aguilas"
    )
  ),
  farm_code = LETTERS[1:7] %>% rev(.)) %>%
  write_csv('data/farm_centroids.csv')


farm_centroids_sf <- 
  read_csv('data/farm_centroids.csv', show_col_types = F) %>% 
  st_as_sf(coords = c("X", "Y"),
           crs = st_crs(4326)) %>% 
  mutate(distance_to_coast = st_distance(farm_centroids_sf, costa) %>% as.numeric()/1e3)


st_write(farm_centroids_sf, 'data/farm_centroids_sf.shp', append=FALSE)

# units::set_units(km^2)
glimpse(recintos_buffer)
mapview::mapview(recintos_buffer)
st_write(recintos_buffer, 'data/recintos_buffer.shp', append = FALSE)


