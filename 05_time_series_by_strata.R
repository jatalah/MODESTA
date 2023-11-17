rm(list = ls())
library(stars)
library(timetk)
library(ggpubr)

load("time_series_by_strata.RData")

farm_codes <-
  tibble(
    name = c(
      "Aguilas",
      "EL Gorguel",
      "San Pedro",
      "Guardamar",
      "El Campello",
      "La Vila Joiosa",
      "Calp y Altea"
    ),
    farm_code = rev(c("A", "B", "C", "D", "E", "F", "G"))
  ) %>%
  write_csv('data/farm_codes.csv')

# read farm layers------
recintos_buffer <- st_read('data/recintos_buffer.shp')

farm_centroids_sf <- 
  read_csv('data/farm_centroids.csv') %>% 
  st_as_sf(coords = c("X", "Y"),
           crs = st_crs(4326)) 

# read the nc data using the stars library------------ 
read_func <- 
  function(path) {
  read_ncdf(path,
            var = c("thetao"),
            make_units = F) %>%
    as_tibble() %>%
    drop_na() %>%
    filter(depth==min(depth)) %>% 
    st_as_sf(coords = c("lon", "lat"),
             crs = st_crs(4326)) %>%
    st_intersection(recintos_buffer) %>%
    mutate(
      longitude = sf::st_coordinates(.)[, 1],
      latitude = sf::st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    mutate(depth = as.numeric(depth))
  }

# read data--------
ts_strata <-
  bind_rows(
    Surface = read_func(path = 'data/thetao_1987_2020_0_10m.nc'),
    `10 m` = read_func(path = 'data/thetao_1987_2020_10_20m.nc'),
    `20 m` = read_func(path = 'data/thetao_1987_2020_20_30m.nc'),
    `30 m` = read_func(path = 'data/thetao_1987_2020_30_40m.nc'),
    .id = 'Depth'
  )

write_rds(ts_strata, 'data/ts_strata.rds')

# read new data -----
new_d <-
  read_ncdf('data/med_thetao_2020_2022.nc',
            var = c("thetao"),
            make_units = F) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(
    between(depth, 1, 2) |
      between(depth, 10, 11) |
      between(depth, 22, 23) | 
      between(depth, 29, 30)
  ) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = st_crs(4326)) %>%
  st_intersection(recintos_buffer) %>%
  # st_join(farm_centroids_sf, join = st_nearest_feature, left = T) %>%
  mutate(
    longitude = sf::st_coordinates(.)[, 1],
    latitude = sf::st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  mutate(
    depth = as.numeric(depth),
    Depth = case_when(
      between(depth, 0, 2) ~ "Surface",
      between(depth, 9, 11) ~ "10 m",
      between(depth, 20, 25) ~ "20 m",
      between(depth, 20, 30) ~ "30 m"
    )
  ) %>%
  select(-latitude, -longitude)


# ts_strata_new <- bind_rows(ts_strata, new_d)

write_csv(ts_strata, 'data/ts_strata.csv')

# group and summarise by farm---------
ts_strata_farm <-
  ts_strata %>%
  group_by(Depth, depth, time, name, farm_code) %>%
  summarise(thetao = mean(thetao)) %>%
  ungroup() %>%
  # mutate(year = year(time),
  #        month = month(time),
  #        day = yday(time)) %>%
  # group_by(depth, name, month) %>%
  # mutate(thetao_hist_mean = mean(thetao[year <= 2010])) %>% 
  # group_by(depth, name) %>% 
  # mutate(
  #   pr_anom = (thetao * 100 / thetao_hist_mean) - 100, # pr anomaly as a percentage centered at zero
  #   ta_anom = thetao - thetao_hist_mean # straight anomaly
  # ) %>% 
  mutate(Depth = fct_relevel(Depth, "Surface", "10 m")) # reorder Depth 


# raw time series-----------
ggplot(ts_strata_farm, aes(as_date(time), thetao, color = farm_code)) +
  geom_line() +
  theme_minimal() +
  labs(x = NULL, y = "Monthly temperature average (°C)") +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  facet_wrap(~Depth, ncol = 1)


# seasonal trends -----------
ts_strata_farm %>% 
  mutate(month = month(time, label = F)) %>% 
  ggplot(aes(x = factor(month), y = thetao)) +
  geom_boxplot() +
  facet_grid(name~Depth) +
  theme_minimal() +
  labs(x = NULL)

# seasonal plot--------------
seasonal_plot <- 
ts_strata_farm %>% 
  mutate(month = month(time, label = T)) %>% 
  ggplot(aes(x = factor(month), y = thetao, color = farm_code)) +
  geom_violin(alpha = .5, scale = 'width', linewidth = .2) +
  facet_wrap(~Depth, ncol = 1) +
  theme_minimal(base_size = 8) +
  labs(x = NULL, y = 'Temperature (°C)') +
  scale_color_tableau(name = NULL) +
  theme(legend.position = 'bottom',
        legend.key.size = unit(0.4, "cm")) +
  guides(colour = guide_legend(nrow = 1))

seasonal_plot

# plot the raw monthly average by depth-----------
monthly_averages <- 
  ts_strata_farm %>% 
  mutate(month = as_date(floor_date(time, 'month'))) %>% 
  group_by(name, farm_code, Depth, month) %>% 
  summarise(monthly_mean_thetao = mean(thetao, na.rm = T))

ggplot(monthly_averages, aes(month, monthly_mean_thetao, color = Depth)) +
  geom_line() +
  theme_minimal() +
  labs(x = NULL, y = "Monthly temperature average (°C)") +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  facet_wrap(~Depth, ncol = 1)

# deseasonalise data and obtain anomalies ------
trends_thetao <- 
  monthly_averages %>%
  drop_na() %>% 
  group_by(name, farm_code, Depth) %>%
  tk_anomaly_diagnostics(month, monthly_mean_thetao) %>%
  ungroup() 

# # deseasonalise daily data and obtain anomalies ------
# trends_thetao_daily <- 
#   ts_strata_farm %>%
#   drop_na() %>% 
#   group_by(name, farm_code, Depth) %>%
#   tk_anomaly_diagnostics(time, thetao) %>%
#   ungroup()
# 
# 
# trends_thetao_daily %>%
#   ggplot(aes(as_date(time), runmed(seasadj, 11), color = name)) +
#   geom_line(alpha = .9) +
#   theme_minimal(base_size = 8) +
#   geom_smooth(method = lm) +
#   labs(x = NULL, y = "Trend in monthly temperature average (°C)") +
#   scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
#   facet_wrap( ~ Depth, ncol = 1, scale = 'free') +
#   scale_color_tableau(name = NULL) +
#   theme(legend.position = 'bottom',
#         legend.key.size = unit(0.4, "cm"))

# monhtly trends--------
trends_plot <- 
trends_thetao %>%
  ggplot(aes(as_date(month), trend, color = farm_code)) +
  geom_line(alpha = .9) +
  theme_minimal(base_size = 8) +
  labs(x = NULL, y = "Trend in monthly temperature average (°C)") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  facet_wrap( ~ Depth, ncol = 1, scale = 'free') +
  scale_color_tableau(name = NULL) +
  # scale_color_viridis_d(name = NULL, end = .8) +
  theme(legend.position = 'bottom',
        legend.key.size = unit(0.4, "cm")) +
  guides(colour = guide_legend(nrow = 1))

trends_plot


# Figure 2 long-term and seasonal trends--------------
figure_trends <-
  ggarrange(
    trends_plot,
    seasonal_plot,
    common.legend = T,
    legend = 'bottom',
    labels = 'auto'
  )

figure_trends

ggsave(
  figure_trends,
  filename = 'figures/figure2_ts_trends_plots.svg',
  width = 7,
  height = 6,
  dpi = 300,
  bg = 'white'
)
