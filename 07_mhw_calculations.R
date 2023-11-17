rm(list = ls())
library(ggpubr)
library(heatwaveR)
library(sf)
library(stars)
sf_use_s2(FALSE)

load('mhw_calculations.RData')

# read farm layers------
recintos_buffer <- st_read('data/recintos_buffer.shp')

farm_centroids_sf <- 
  read_csv('data/farm_centroids.csv') %>% 
  st_as_sf(coords = c("X", "Y"),
           crs = st_crs(4326)) 

# read SST data for each farm--------------- 
sst_farms <- 
read_ncdf('data/stt_med_all.nc',
          var = "analysed_sst",
          make_units = F) %>%
  st_crop(recintos_buffer) %>%
  as_tibble() %>%
  drop_na() %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = st_crs(4326)) %>%
  st_join(farm_centroids_sf, join = st_nearest_feature, left = T) %>%
  mutate(
    longitude = sf::st_coordinates(.)[, 1],
    latitude = sf::st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  as_tibble() %>% 
  mutate(analysed_sst = analysed_sst -273.15) %>% 
  group_by(time, name, farm_code) %>% 
  summarise(analysed_sst = mean(analysed_sst))

# nest data and calculate climatology and MHW events--------------
ts_nest <-
  sst_farms %>%
  group_by(farm_code) %>%
  select(time, analysed_sst) %>%
  mutate(time = as_date(time)) %>%
  rename(t = time, temp = analysed_sst) %>%
  nest() %>%
  mutate(
    ts_clim = map(data, ~ ts2clm(.x, climatologyPeriod = c("1981-08-25", "2011-08-24"))),
    tsd_mhw = map(ts_clim, ~ detect_event(.x)),
    events = map(tsd_mhw, ~ .x$event),
    clima = map(tsd_mhw, ~ .x$climatology)
  )

# unnest the events data---------------------
event_d <- 
ts_nest %>% 
  select(events) %>% 
  unnest(cols = events) %>% 
  ungroup() %>% 
  mutate(month_peak = month(date_peak))

# unnest the climatology data---------------------
clima_d <- 
  ts_nest %>% 
  select(clima) %>% 
  unnest(cols = clima) %>% 
  ungroup()

# Height of lollis represent event durations and the top five (longest)
# lollis are highlighted in red:
ggplot(event_d, aes(x = date_peak, y = duration)) +
  geom_lolli(colour = "salmon",
             colour_n = "red",
             n = 3) +
  theme_minimal() +
  facet_wrap(~farm_code, ncol = 1, scales = 'free') +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  labs(x = NULL, y = 'Event duration (days)')


# MHWs figure ----------------------------
# Height of lollis represent event durations and their colours
# are mapped to the events' cumulative intensity:
ggplot(event_d, aes(x = date_peak, y = duration)) +
  geom_lolli(aes(colour = intensity_cumulative)) +
  scale_color_distiller(
    palette = "Reds",
    direction = 1,
    name = "Cumulative \nintensity",
    trans = 'log10'
  ) +
  # scale_color_viridis_c(option = "H", name = "Cumulative \nintensity", trans = 'log10') +
  labs(x = NULL, y = "Event duration (days)") +
  facet_wrap( ~ farm_code, ncol = 1, scales = 'fixed') +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  scale_y_continuous() +
  theme_minimal(base_size = 9) +
  theme(legend.position = 'bottom',
        legend.key.size = unit(0.75, "cm"))

ggsave(
  last_plot(),
  filename = 'figures/figure_4.svg',
  width = 6,
  height = 8,
  dpi = 300,
  bg = 'white'
)

clima_d_latest <- 
clima_d %>% 
  filter(year(t)>2021)

ggplot(data =  clima_d_latest , aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = T) +
  # geom_flame(
  #   data =  clima_d_latest,
  #   aes(y = temp, y2 = thresh, fill = "top"),
  #   show.legend = T
  # ) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = .5) +
  geom_line(aes(y = seas, colour = "seas"), size = .5) +
  scale_colour_manual(
    name = "Line Colour",
    values = c(
      "temp" = "black",
      "thresh" =  "forestgreen",
      "seas" = "grey80"
    )
  ) +
  scale_fill_manual(name = "Event Colour",
                    values = c("all" = "salmon",
                               "top" = "red")) +
  scale_x_date(date_labels = "%b %Y") +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature (", degree, "C)")), x = NULL) +
  theme_minimal() +
  facet_wrap(~farm_code, ncol = 1, scales = 'fixed')

ggsave(
  last_plot(),
  filename = 'figures/figureS1_mhw_events_2022.svg',
  width = 6,
  height = 8,
  dpi = 300,
  bg = 'white'
)


ggplot(event_d, aes(farm_code, intensity_max)) +
  geom_boxplot()

# MHW summary stats ---------
event_d %>%
  group_by(decade = floor_date(date_peak, years(10))) %>%
  summarise(mean_number_of_events_per_year = n()/n_distinct(year(date_peak)),
            mean_cummulative_intensity_per_event = mean(intensity_cumulative),
            mean_intensity_per_event = mean(intensity_mean),
            max_intensity_per_event = mean(intensity_max),
            mean_duration_per_event = mean(duration),
            max_duration = max(duration))


skimr::skim(event_d)

event_d %>% 
  group_by(decade = floor_date(date_peak, years(10))) %>%
  filter(duration == max(duration)) %>% 
  select(duration, farm_code, contains('date')) %>% 
  arrange(decade)


# summer 2022 MHW---
event_d %>% 
filter(date_start>as_date("2022-01-01")) %>%
  group_by(farm_code) %>% 
  filter(duration == max(duration)) %>% 
  arrange(farm_code) %>% summary

