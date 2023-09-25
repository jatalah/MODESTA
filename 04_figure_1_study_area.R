library(tidyverse)
library(sf)
library(stars)
library(ggspatial) # for north arrow
library(cowplot)
library(rnaturalearth)
library(metR)
sf_use_s2(FALSE)
rm(list = ls())

# define geographical study area--------------  
bbox <- st_bbox(c(xmin = -1.65, ymin = 37.2,  xmax = 0.2, ymax = 38.65), crs = st_crs(4326))

# read data ---------
recintos_buffer <- st_read('data/recintos_buffer.shp')

recintos <-
  read_sf('C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/acuivisor_data/Recintos.shp') %>%
  st_crop(st_bbox(c(
    xmin = -1.9,
    ymin = 37.374720,
    xmax = 1.11,
    ymax = 38.73
  )))

farm_centroids_sf <-
  read_csv('data/farm_centroids.csv') %>%
  st_as_sf(coords = c("X", "Y"),
           crs = st_crs(4326))

# read the bathymetry tif--------- 
bathy <- 
  read_stars('data/gebco_2022_n42.4292_s34.4751_w-6.6577_e3.0981.tif', crs = st_crs(4326), make_units = F) %>% 
  st_crop(bbox) %>% 
  rename(depth = 1) %>% 
  .[.<5] %>% 
  # mutate(depth = depth *-1) %>% 
  as_tibble() %>% 
  mutate(depth =- as.numeric(depth)) %>% 
  drop_na()

# read the coast line shapefile------------
coast <- 
  read_sf('data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>% 
  st_transform(crs = st_crs(4326)) %>% 
  st_crop(bbox) %>% 
  st_union()

# st_write(coast, 'data/coast_study_area.shp')

# vector of major city names-------------
cities <- 
tibble(
  name = c("Alacant", "Murcia", "Cabo de Palos"),
  x = c(-0.4902652373938165,-1.1326953624950447, -0.69),
  y = c(38.34629587269519, 37.99298798143878, 37.625),
)

# Map of study area------
sites_map <-
  ggplot() +
  geom_raster(data = bathy, aes(x, y, fill = depth)) +
  scale_fill_viridis_c(option = 'G',
                       name = "Depth (m)",
                       direction = -1, trans = 'log10', begin = .05, end = .99) +
  geom_sf(
    data = recintos_buffer,
    fill = 'transparent',
    color = 'gray50',
    linewidth = .5
  ) +
  geom_sf_label(
    data = recintos_buffer,
    aes(label = farm_code),
    size = 2.5,
    nudge_x = .08,
    nudge_y = -.07,
    fill = 'white',
    alpha = .7,
    color = 'gray20'
  ) +
  geom_sf(data = recintos,
          color = 'darkred',
          linewidth = .5) +
  geom_sf(data = coast) +
  coord_sf(expand = F) +
  geom_contour(
    data = bathy,
    aes(x, y, z = depth),
    color = 'gray70',
    linewidth = .1
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.15,
    bar_cols = 'white',
    style  = 'ticks'
  ) +
  geom_point(data = cities,
             aes(x, y),
             size = 2,
             alpha = .7) +
  geom_text(
    data = cities,
    aes(x, y - .025, label = name),
    size = 3,
    alpha = .7
  ) +
  labs(x = NULL, y = NULL)

sites_map

# inset map of Iberian peninsula---------------
iberian_map <- 
ggplot() + 
  geom_sf(data = ne_countries(
    scale = 'large',
    type = 'map_units',
    returnclass = 'sf',
    continent = "Europe"
  ), fill = 'gray60', color = "white") +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    panel.border = element_rect(color = 'gray30', fill= NA)
  ) +
  geom_point(aes(y= 37.570215, x  = -0.873569), size = 2, color =2) +
  coord_sf(
    xlim = c(-10,4.5),
    ylim = c(35.5, 44.3),
    expand = FALSE
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    # pad_x = unit(1, "cm"),
    # pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering,
    height = unit(.75, "cm"),
    width = unit(.75, "cm")
  )

# put togeteher and save figure 1-----------------
fig_map <- 
  cowplot::ggdraw() +
  draw_plot(sites_map) +
  draw_plot(
    iberian_map,
    x = 0.15,
    y = .75,
    width = .2,
    height = .2
  )

fig_map

ggsave(plot = fig_map,
       device = 'svg',
       filename = "figures/fig_1_study_sites.svg",
       width = 7, 
       height = 5,
       bg = "white",
       dpi = 600)
