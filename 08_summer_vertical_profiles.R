rm(list = ls())
library(sf)
library(s2)
library(mapview)
library(stars)
library(akima)
library(metR) # for contours
library(geomtextpath) # for contours
library(ggfx) # for contour labels 
sf_use_s2(FALSE)


load('summer_vertical_profiles.RData')

# read farm layers----------
recintos_buffer <- read_sf('data/recintos_buffer.geojson')
names <- recintos_buffer %>% distinct(name) %>% deframe()

mapview(recintos_buffer)

# helper functions-------
prep_data_func <- function(path) {
  read_ncdf(path,
            var = c("thetao"),
            make_units = F) %>%
    st_crop(recintos_buffer) %>%
    as_tibble() %>%
    drop_na() %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(4326)) %>%
    st_join(recintos_buffer, join = st_nearest_feature, left = T) %>%
    mutate(
      longitude = sf::st_coordinates(.)[, 1],
      latitude = sf::st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    mutate(depth = as.numeric(depth)) %>%
    group_by(name, depth, day = yday(time)) %>%
    summarise(thetao = mean(thetao, na.rm = TRUE), .groups = 'drop') %>%
    group_by(name) %>%
    nest() %>%
    mutate(profile_inter = map(data, ~ interp2xyz(
      interp(
        x = .x$day,
        y = .x$depth,
        z = .x$thetao,
        duplicate = "mean",
        nx = 200,
        ny = 200,
        linear = FALSE
      ),
      data.frame = TRUE
    ) %>%
      as_tibble())) %>%
    select(name, profile_inter) %>%
    unnest(cols = profile_inter) %>%
    ungroup() %>%
    drop_na()
}

profile_plot_func <-
  function(data) {
    ggplot(data, aes(x, y, fill  = z)) +
      geom_raster() +
      scale_y_reverse(limits = c(50, 0)) +
      labs(y = 'Depth (m)',
           x = NULL) +
      facet_wrap(~ farm_code, scale = 'free_y', ncol = 1) +
      scale_fill_viridis_c(option = "H", name = "ºC") +
      with_outer_glow(
        geom_textcontour(aes(z = z), linetype = NA, size = 3),
        colour = 'white',
        expand = 3,
        sigma = 1
      ) +
      geom_textcontour(aes(z = z), textcolour = NA)  +
      theme_minimal(base_size = 9) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_continuous(
        breaks = c(152, 182, 213, 244, 274),
        labels = c("01-Jun", "01-Jul", "01-Aug", "01-Sept", "01-Oct")
      )
  }

# summer 2022
dat_22 <- prep_data_func("data/temp_2022_jun_sept.nc") %>%  left_join(read_csv('data/farm_codes.csv'))

plot_22 <- profile_plot_func(dat_22)

print(plot_22)

ggsave(
  plot_22,
  filename = 'figures/figure_5.svg',
  width = 7 ,
  height = 7,
  dpi = 600, 
  bg = 'white'
)

# summer 2021
# dat_21 <- prep_data_func("data/temp_2021_jun_sept.nc")
# plot_21 <- profile_plot_func(dat_21)
# print(plot_21)
# 
# ggsave(
#   plot_21,
#   filename = 'figures/profiles_2021.png',
#   width = 7 ,
#   height = 7,
#   dpi = 150, 
#   bg = 'white'
# )

dat_22 %>%
  group_by(farm_code, jday = round(x, 0), y) %>%
  summarise(z = mean(z, na.rm = TRUE), .groups = 'drop') %>%
  group_by(farm_code, jday) %>%
  filter(z > 26) %>%
  slice_max(y, n = 1) %>% ungroup() %>% skim()
  ggplot(aes(jday, y, color = farm_code)) +
  geom_line() +
  scale_y_reverse() +
  theme_minimal(base_size = 9) +
  scale_x_continuous(
    breaks = c(152, 182, 213, 244, 274),
    labels = c("01-Jun", "01-Jul", "01-Aug", "01-Sept", "01-Oct")
  ) +
  theme(legend.position = 'bottom') +
  labs(x = NULL, y = 'Depth (m)')

dat_22 %>% filter(z>29) %>% distinct(farm_code)
