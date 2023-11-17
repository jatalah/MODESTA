rm(list = ls())
load("C:/Users/javiera/OneDrive - Cawthron/UA/MODESTA/spatial_trends_by_strata.RData")
library(automap) 
library(stars)
library(broom)
library(ggpubr)
library(skimr)
library(rnaturalearth)
theme_set(theme_minimal())
sf_use_s2(FALSE)

# library(rnaturalearth)
bbox <-
  st_bbox(c(
    xmin = -1.65,
    ymin = 37.2,
    xmax = 0.2,
    ymax = 38.65
  ), crs = st_crs(4326))
recintos_buffer <- st_read('data/recintos_buffer.shp')

farm_centroids_sf <- 
  st_read('data/farm_centroids_sf.shp') %>% 
    rename(farm_code = farm_cd,
           distance_to_coast = dstnc__)

costa <- 
  read_sf('data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>% 
  st_transform(crs = st_crs(4326)) %>% 
  st_union() %>% 
  st_crop(bbox)

# function to read and calculate sens slopes----
trend_func <- function(path) {
  read_ncdf(path,
            var = 'thetao',
            make_units = F) %>%
    filter(month(time) == 8) %>%
    as_tibble() %>%
    drop_na() %>%
    mutate(year = year(time),
           month = month(time)) %>%
    group_by(lon, lat, year) %>%
    summarise(thetao = mean(thetao)) %>%
    group_by(lat, lon) %>%
    nest() %>%
    mutate(sen = map(data, ~ er.helpers::sen_slope(.x$thetao))) %>%
    dplyr::select(sen) %>%
    unnest(cols = c('sen')) %>%
    ungroup()
}


# apply function to each strata---------
d_sen_surface <- trend_func(path = 'data/thetao_1987_2020_0_10m.nc')
d_sen_10 <- trend_func(path = 'data/thetao_1987_2020_10_20m.nc')
d_sen_20 <- trend_func(path = 'data/thetao_1987_2020_20_30m.nc')
d_sen_30 <- trend_func(path = 'data/thetao_1987_2020_30_40m.nc')


sen_all_raw <-
  bind_rows(
    Surface =  d_sen_surface,
    `10 m` = d_sen_10,
    `20 m` = d_sen_20,
    `30 m` = d_sen_30,
    .id = 'depth'
  ) %>%
  mutate(depth = fct_relevel(depth, "Surface", "10 m", "20 m"))


sen_all_raw %>% 
  select(depth, slope) %>% 
  group_by(depth) %>% 
  skim() 


ggplot(sen_all_raw, aes(depth, slope)) + geom_boxplot()

# p-values-------

ggplot(sen_all_raw) +
  geom_raster(aes(lon, lat, fill = p_value)) +
  facet_wrap( ~ depth) +
  scale_fill_viridis_c(option = 'A') +
  geom_sf(data = costa) +
  geom_sf(data = recintos_buffer, fill = 'transparent', color = "red")

# z values-----
ggplot(sen_all_raw) +
  geom_tile(aes(lon, lat, fill = z)) +
  facet_wrap( ~ depth) +
  scale_fill_viridis_c(option = 'A') +
  geom_sf(data = costa) +
  geom_sf(data = recintos_buffer, fill = 'transparent', color = "red")

# z values-----
ggplot(sen_all_raw) +
  geom_tile(aes(lon, lat, fill = conf_low)) +
  facet_wrap( ~ depth) +
  scale_fill_viridis_c(option = 'A') +
  geom_sf(data = costa) +
  geom_sf(data = recintos_buffer, fill = 'transparent', color = "red")

# krigging function------
krig_func <- function(data) {
  data %>%
    select(lat, lon, slope) %>%
    drop_na() %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    as_Spatial() %>%
    autoKrige(slope ~ 1, input_data = ., new_data = pred_grid)
}

# apply krigging function---
pred_grid <-
  expand_grid(lon = seq(-1.65, 0.2, 0.01), lat = seq(37.2, 38.65, .01)) %>%
  st_as_stars()

krig_sen_surface <- krig_func(d_sen_surface)
krig_sen_10 <- krig_func(d_sen_10)
krig_sen_20 <- krig_func(d_sen_20)
krig_sen_30 <- krig_func(d_sen_30)


# pred_grid <-
#   st_make_grid(st_as_sfc(bbox)) %>%
#   st_difference(costa) %>%``
#   st_as_stars()

d_sen_plot <- 
bind_rows(
  Surface = krig_sen_surface$krige_output %>% as_tibble(),
  `10 m` = krig_sen_10$krige_output %>% as_tibble(),
  `20 m` = krig_sen_20$krige_output %>% as_tibble(),
  `30 m` = krig_sen_30$krige_output %>% as_tibble(),
  .id = 'depth'
) %>% 
  mutate(depth = fct_relevel(depth, "Surface", "10 m", "20 m"))

d_sen_sf <- 
d_sen_plot %>% 
  st_as_sf(coords = c("x", "y"),
           crs = st_crs(4326)) %>% 
  st_difference(costa) %>% 
  st_crop(bbox)

# Figure Map sen slopes-------------------
map_sen <- 
ggplot(d_sen_plot) +
  geom_raster(aes(x = x, y = y, fill = var1.pred * 10))  +
  geom_sf(data = costa) +
  geom_sf(data = recintos_buffer, fill = 'transparent', color = "white") +
  geom_sf_text(
    data = recintos_buffer,
    aes(label = farm_code),
    size = 2,
    # nudge_x = -.15,
    # nudge_y = .05,
    # fill = 'white',
    alpha = .7,
    color = 'gray20'
  ) +
  scale_fill_viridis_c(option = 'A', name = "°C per decade") +
  labs(y = NULL, x = NULL) +
  theme_void(base_size = 8) +
  coord_sf(expand = F, xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2],bbox[4])) +
  facet_wrap( ~ fct_relevel(depth, "Surface"), nrow = 2) +
  theme(legend.position = 'bottom',
              legend.key.size = unit(0.5, "cm"),
        panel.border = element_rect(colour = 'gray70', fill = 'transparent'))

map_sen

# save plot------
ggsave(
  map_sen,
  filename = 'figures/figure_3.svg',
  width = 4,
  height = 4,
  dpi = 600,
  bg = 'white'
)

# SD of the sen slope
ggplot() +
  geom_sf(data = d_sen_sf, aes( color = var1.stdev * 10)) +
  facet_wrap(~depth) +
  scale_color_viridis_c(option = 'A', trans = 'log10') +
  geom_sf_text(
    data = recintos_buffer,
    aes(label = farm_code),
    size = 2,
    # nudge_x = -.15,
    # nudge_y = .05,
    # fill = 'white',
    alpha = .7,
    color = 'gray20'
  ) 


## Sen for farm buffer only------------
sen_by_farm <-
  d_sen_sf %>% 
  st_intersection(recintos_buffer) %>% 
  bind_cols(st_coordinates(.) %>% as_tibble()) %>% 
  as_tibble() 

ggplot() +
  geom_sf(data = st_intersection(d_sen_sf, recintos_buffer), aes( color = var1.pred * 10)) +
  facet_wrap(~depth) +
  scale_color_viridis_c(option = 'A')

# sen slope exploration ------
ggplot(sen_by_farm, aes(fct_relevel(depth, "Surface"), var1.pred, fill = farm_code)) + 
  geom_boxplot(alpha = .7) +
  theme_minimal() + 
  labs(y = "°C per decade", x = "Depth", fill = NULL) +
  theme(legend.position = 'right')


# sen vs distance to coast plot ------------
sen_by_farm %>% 
  group_by(farm_code, depth) %>% 
  summarise(mean_sen = mean(var1.pred), .groups = 'drop') %>% 
  left_join(farm_centroids_sf) %>% 
  ggplot(aes(distance_to_coast, mean_sen, color = fct_relevel(depth, "Surface"))) +
  geom_point() +
  geom_smooth(se = F, method = lm, formula = y ~ poly(x, 2), linewidth = .5) +
  labs(x = "Distance to the coast (km)", y = "Mean sen slope", color = NULL) +
  theme_minimal() +
  theme(legend.position = 'bottom')


# Sen vs latitude plot -------
sen_by_farm %>% 
  group_by(farm_code, depth) %>% 
  summarise(mean_sen = mean(var1.pred), .groups = 'drop',
            x = mean(Y),
            y = mean(Y)) %>% 
  left_join(farm_centroids_sf) %>% 
  ggplot(aes(y, mean_sen, color = fct_relevel(depth, "Surface"))) +
  geom_point() +
  geom_smooth(se = F, method = lm, formula = y ~ poly(x, 2), linewidth = .5) +
  labs(x = "Latitude", y = "Mean sen slope", color = NULL) +
  theme_minimal() +
  theme(legend.position = 'bottom')

# save plot------
ggsave(
  last_plot(),
  filename = 'figures/sen_latitude_plot.png',
  width = 4,
  height = 4,
  dpi = 600,
  bg = 'white'
)

# sen by farm summaries ---
sen_by_farm %>% 
  group_by(farm_code) %>% 
  summarise(mean_sen = mean(var1.pred),
            sd_sen = sd(var1.pred),
            .groups = 'drop') %>% 
  arrange(-mean_sen)

sen_by_farm %>% 
  select(depth, var1.pred) %>% 
  group_by(depth) %>%
  skim() %>% 
  yank('numeric') %>% 
  as_tibble() %>% 
  arrange(-mean)

# d_sen_farms <- 
#   d_sen_sf %>% 
#   st_join(farm_centroids_sf, join = st_nearest_feature, left = T) %>%
#   mutate(
#     longitude = sf::st_coordinates(.)[, 1],
#     latitude = sf::st_coordinates(.)[, 2]
#   ) %>%
#   st_drop_geometry() %>%
#   as_tibble()
# 
# d_sen_sf %>% 
#   group_by(depth) %>% 
#   summarise(mean(var1.pred)*10)

  
