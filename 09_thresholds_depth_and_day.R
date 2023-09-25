rm(list = ls())
library(sf)
library(stars)
library(ggpubr)
library(skimr)
library(ggpmisc)
library(broom)
sf_use_s2(FALSE)

load('depth_at_25.RData')

# read farm layers------------ 
recintos_buffer <- st_read('data/recintos_buffer.shp')

farm_centroids_sf <- st_read('data/farm_centroids_sf.shp')

read_func <- function(path, thr) {
  read_ncdf(path,
            var = c("thetao"),
            make_units = F) %>%
    st_crop(recintos_buffer) %>%
    as_tibble() %>%
    drop_na() %>%
    group_by(time, lat, lon) %>%
    filter(thetao > thr) %>%
    slice_max(depth, n = 1) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = st_crs(4326)) %>%
    st_join(farm_centroids_sf, join = st_nearest_feature, left = T) %>%
    mutate(
      longitude = sf::st_coordinates(.)[, 1],
      latitude = sf::st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry() %>%
    as_tibble()
}
  
# read and process data--------
# new_d_26 <- read_func(path = 'data/med_thetao_2020_2022.nc', thr = 26)
# new_d_25 <- read_func(path = 'data/med_thetao_2020_2022.nc', thr = 25)

d_26 <-
  bind_rows(
    read_func(path = 'data/thetao_1987_2020_0_10m.nc', thr = 26),
    read_func(path = 'data/thetao_1987_2020_10_20m.nc', thr = 26),
    read_func(path = 'data/thetao_1987_2020_20_30m.nc', thr = 26),
    read_func(path = 'data/thetao_1987_2020_30_40m.nc', thr = 26)
  )

d_25 <-
  bind_rows(
    read_func(path = 'data/thetao_1987_2020_0_10m.nc', thr = 25),
    read_func(path = 'data/thetao_1987_2020_10_20m.nc', thr = 25),
    read_func(path = 'data/thetao_1987_2020_20_30m.nc', thr = 25),
    read_func(path = 'data/thetao_1987_2020_30_40m.nc', thr = 25)
  )


sum_func <- function(data, th){data %>% 
    group_by(time, longitude, latitude) %>%
    filter(thetao >= th) %>%
    slice_max(depth, n = 1) %>% 
    group_by(name, time) %>%
    summarise(depth = max(depth)) %>% 
    ungroup() }
  
d_26_sum <- sum_func(d_26, 26)
d_25_sum <-  sum_func(d_25, 25)
# new_d_25_sum <- sum_func(new_d_25 , 25)
# new_d_26_sum <- sum_func(new_d_25 , 26)

# bind_rows(d_26_sum, new_d_26_sum)

d_all <-
  bind_rows(
    # `25°C` = bind_rows(d_25_sum, new_d_25_sum),
    # `26°C` = bind_rows(d_26_sum, new_d_26_sum),
    `25°C` = d_25_sum,
    `26°C` = d_26_sum,
    .id = 'Thr'
  ) %>%
  mutate(month = month(time),
         year = year(time),
         day = yday(time)) %>% 
  left_join(read_csv('data/farm_codes.csv')) %>% 
  write_rds('data/d_depth_all_farms.rds')


d_all <- read_rds('data/d_depth_all_farms.rds')

depth_d <- 
d_all %>% 
  group_by(year, Thr, farm_code) %>% 
  summarise(depth = max(depth)) 

plot_depth <- 
  depth_d %>% 
  ggplot(aes(year, depth, color = Thr)) +
  geom_line(alpha = .5, linewidth = .5) +
  # geom_point(alpha = .25, size = .5) +
  geom_smooth(se = F, linewidth = .25, method = 'lm') +
  facet_wrap(~farm_code, ncol = 1) +
  scale_y_reverse() +
  theme_minimal(base_size = 8) +
  scale_color_economist(name = NULL) +
  theme(legend.position = 'bottom') +
  labs(x = NULL, y = "Depth (m)")

plot_depth


# summary of thresholds depth------------
depth_d %>% group_by(Thr) %>% skim()

depth_d %>%
  group_by(Thr, farm_code) %>% 
  select(depth) %>% 
  skim_without_charts() %>% 
  as_tibble() %>% 
  filter(Thr=="26°C") %>% 
  select(farm_code, numeric.mean)


# LMS of thresholds depth trends--------
depth_lms <- 
  depth_d %>%
  group_by(Thr, farm_code) %>% 
  nest() %>% 
  mutate(lms = map(data, ~lm(depth~year, .x)),
         lms_tidy = map(lms, ~tidy(.x)),
         lms_glance = map(lms, glance)) 


depth_lms %>% 
  select(lms_tidy) %>% 
  unnest(c(lms_tidy)) %>%
  filter(term == "year") %>% 
  mutate(estimate = estimate *-1)

# Seasonal onset --------------------------
day_d <- 
  d_all %>% 
  group_by(year, Thr, farm_code) %>% 
  summarise(day = min(day))

day_d %>%
  group_by(Thr, farm_code) %>% 
  select(day) %>% 
  skim()

day_lms <- 
day_d %>% 
  group_by(Thr, farm_code) %>% 
  nest() %>% 
  mutate(lms = map(data, ~lm(day~year, .x)),
         lms_tidy = map(lms, tidy),
         lms_glance = map(lms, glance)) 


day_lms %>% 
  select(lms_tidy) %>% 
  unnest(c(lms_tidy)) %>% 
  filter(term == "year") %>% 
  select(Thr, farm_code, estimate) %>% 
  group_by(Thr) %>% 
  skim()


# predictions ----------
day_lms %>% 
  mutate(preds = map(lms, ~predict(.x , newdata = tibble(year = 2050)))) %>% 
  select(preds) %>% 
  unnest(preds) %>%  
  group_by(Thr) %>% 
  skimr::skim()

plot_day <-
  day_d %>%
  ggplot(aes(year, day, color = Thr)) +
  geom_line(alpha = .5, linewidth = .5) +
  geom_smooth(se = F , linewidth = .25, method = 'lm')  +
  facet_wrap( ~ farm_code, ncol = 1) +
  theme_minimal(base_size = 8) +
  scale_color_economist(name = NULL) +
  theme(legend.position = 'bottom') +
  labs(x = NULL, y = "Julian day")

plot_day


# save both plot together------
ggarrange(plot_depth,
          plot_day,
          ncol = 2,
          common.legend = T)

ggsave(
  last_plot(),
  filename = 'figures/depth_thresholds_plot.png',
  width = 4,
  height = 7,
  dpi = 300,
  bg = 'white'
)


# sen slopes-----
depth_d %>%
  group_by(Thr, farm_code) %>% 
  nest() %>%
  mutate(sen = map(data, ~ er.helpers::sen_slope(.x$depth))) %>%
  dplyr::select(sen) %>%
  unnest(cols = c('sen')) %>%
  ungroup()

day_d %>%
  group_by(Thr, farm_code) %>% 
  nest() %>%
  mutate(sen = map(data, ~ er.helpers::sen_slope(.x$day))) %>%
  dplyr::select(sen) %>%
  unnest(cols = c('sen')) %>%
  ungroup()