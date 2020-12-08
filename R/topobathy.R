
# get depth of transect points from topobathy -----------------------------

library(raster)
library(mapview)
library(tbeptools)
library(sf)
library(tidyverse)
library(lubridate)

utm <- '+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs'

# # https://www.ngdc.noaa.gov/mgg/bathymetry/estuarine/
# dem <- raster('~/Desktop/tampa_bay_G070_2017.nc')
# # dem <- readAll(dem)
# # save(dem, file = 'data/dem.RData', compress = 'xz')

data(dem)

loc <- 'S3T12'
var <- 'Abundance'
spp <- c('Halodule', 'Syringodium', 'Thalassia', 'Ruppia', 'Halophila')

# subset transct, get pa for five major seagrass speies
trn <- transect %>% 
  dplyr::filter(Transect %in% loc) %>%  
  dplyr::filter(var %in% 'Abundance') %>% 
  dplyr::filter(Savspecies %in% !!c(spp, 'No Cover')) %>% 
  group_by(Transect, Site, Date, Depth_obs = Depth) %>% 
  summarise(pa = sum(aveval, na.rm = T), .groups = 'drop') %>% 
  mutate(pa = ifelse(pa > 0, 1, 0))

# get transect angle
# location of starting point in UTM
lns <- trnlns %>% 
  st_transform(crs = utm) %>% 
  dplyr::filter(Site %in% loc) %>%
  st_transform(crs = utm) %>% 
  mutate(
    LONG_M = st_coordinates(.)[1, 1], 
    LAT_M = st_coordinates(.)[1, 2]
  ) %>% 
  select(Transect = Site, LONG_M, LAT_M, bearing) %>% 
  st_set_geometry(NULL)

# get location of transect points
# extract depth from dem using locations
tmp <- trn %>% 
  left_join(lns, by = 'Transect') %>% 
  select(Transect, Date, Site, Depth_obs, pa, LAT_Mstr = LAT_M, LONG_Mstr = LONG_M, bearing) %>% 
  mutate(
    Site = as.numeric(Site),
    LONG_M = Site * sin(bearing * pi / 180),
    LAT_M = LONG_M / tan(bearing * pi / 180), 
    LONG_M = LONG_M + LONG_Mstr,
    LAT_M = LAT_M + LAT_Mstr
  ) %>% 
  st_as_sf(coords = c('LONG_M', 'LAT_M'), crs = utm) %>% 
  st_transform(crs = 4326) %>% 
  mutate(
    Depth_dem = raster::extract(dem, .), 
    Depth_dem = 100 * Depth_dem, 
    Date = floor_date(Date, unit = 'month')
  )

# look at a map to see if the locations are correct
mapview(dem) + mapview(tmp) + mapview(trnlns[trnlns$Site %in% loc, ])

# compare field depth vs topobathy depth
ggplot(tmp, aes(x = Depth_obs, y = Depth_dem, colour = factor(Date))) + 
  geom_point(alpha = 0.5, size = 3) +
  scale_colour_viridis_d() + 
  # geom_smooth(method = 'lm', se = F) + 
  theme_minimal() + 
  theme(
    legend.position = 'none'
  ) +
  labs(
    x = 'Depth, field measured (cm)', 
    y = 'Depth, topographic (cm)', 
    title = paste0('Transect ', loc), 
    subtitle = 'Points colored by sample date'
  )

# plot site by depth and p/a over time
ggplot(tmp, aes(y = Depth_dem, x = Site, group = Date)) + 
  geom_line() + 
  geom_point(aes(colour = factor(pa))) + 
  facet_wrap(~Date) +
  scale_colour_manual(values = c('indianred2', 'darkolivegreen4')) + 
  theme_minimal() + 
  theme(
    legend.title = element_blank()
  ) + 
  labs(
    x = 'Transct position (m)', 
    y = 'Depth, topographic (cm)', 
    title = paste0('Transect ', loc), 
    subtitle = 'Points colored by presence/absence'
  )

# see if the transect plot makes sense
show_transect(transect, site = loc, species = spp)

# need to get some idea of max depth edge (maybe quantile regression?)
maxd <- tmp %>% 
  st_set_geometry(NULL) %>% 
  select(Date, Site, pa, Depth_dem)
