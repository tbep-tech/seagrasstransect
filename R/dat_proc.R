library(tbeptools)
library(tidyverse)
library(lubridate)

data(trnpts)
data(tbsegshed)

otbshed <- tbsegshed %>% 
  filter(bay_segment %in% 'OTB')

otbpts <- trnpts[otbshed, ]
trndat <- read_trnjsn(training = F) %>% 
  anlz_trnjsn(training = F)

# OTB transect data, bb only ----------------------------------------------

# get otb transect, fall period, positive depth as neg, abundance only, complete spp by site, date, transect combos
otbtrn <- trndat %>% 
  mutate(
    mo = month(Date)
  ) %>% 
  filter(Transect %in% otbpts$TRAN_ID) %>% 
  filter(mo >= 7 & mo <=12) %>% 
  mutate(
    Depth = as.numeric(Depth),
    Depth = case_when(
      sign(Depth) == 1 ~ -1 * Depth, 
      T ~ Depth
    ), 
    Savspecies = case_when(
      grepl('^AA', Savspecies) ~ 'AA',
      grepl('^DA', Savspecies) ~ 'DA',
      T ~ Savspecies
    ), 
    Transect = factor(Transect, levels = levels(reorder(as.character(otbpts$TRAN_ID), otbpts$LONG_DD)))
  ) %>% 
  filter(var %in% 'Abundance') %>% 
  select(Date, Transect, Site, Depth, SeagrassEdge, Savspecies, bb = aveval) %>% 
  group_by(Date, Transect, Site, Depth, SeagrassEdge, Savspecies) %>% 
  summarise(bb = mean(bb, na.rm = T)) %>% 
  ungroup %>% 
  complete(Savspecies, nesting(Date, Transect, Site, Depth, SeagrassEdge), fill = list(bb = 0)) %>% 
  arrange(Transect, Site, Savspecies)

# coverage estimates
covest <- otbtrn %>% 
  group_by(Date, Transect, Savspecies) %>% 
  nest() %>% 
  mutate(
    est = purrr::map(data, function(data){
      
      foest <- sum(data$bb > 0, na.rm = T) / nrow(data)   
      bbest <- sum(data$bb, na.rm = T) / nrow(data)
      out <- tibble(foest = foest, bbest = bbest)
      
      return(out)
      
    })
  ) %>% 
  select(-data) %>% 
  unnest(est) %>%
  ungroup %>% 
  filter(Savspecies != 'No Cover') %>% 
  mutate(
    mo = month(Date), 
    yr = year(Date), 
    dy = 15,
    Savspecies = factor(Savspecies, levels = rev(c('AA', 'DA', 'Halodule', 'Thalassia', 'Syringodium', 'Ruppia', 'Halophila spp.')))
  ) %>% 
  group_by(Savspecies, mo, yr, dy, Transect) %>% 
  summarise(
    foest = mean(foest), 
    bbest = mean(bbest)
  ) %>% 
  ungroup() %>% 
  unite('Date', yr, mo, dy, sep = '-') %>% 
  mutate(Date = ymd(Date))

save(covest, file = here::here('data', 'covest.RData'), compress = 'xz')

# algae data --------------------------------------------------------------

# file path
xlsx <- '~/Desktop/phyto_data.xlsx'

# load and assign to object
algdat <- read_importphyto(xlsx, download_latest = T) %>%
  filter(yr < 2020) %>% 
  filter(yr >= 1998)

save(algdat, file = here::here('data', 'algdat.RData'), compress = 'xz')

# all data, annually averaged ---------------------------------------------

data(algdat)
data(covest)

# epc annual averages
epcann <- epcdata %>% 
  filter(bay_segment %in% 'OTB') %>% 
  select(epchc_station, yr, tn,sd_raw_m, chla, Sal_Mid_ppth, Temp_Water_Mid_degC) %>% 
  group_by(epchc_station, yr) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  gather('var', 'val', -epchc_station, -yr) %>% 
  rename(station = epchc_station) %>% 
  mutate(
    dat = 'epc',
    station = as.character(station)
  )

# algdata annual averages, average across months
algann <- algdat %>% 
  filter(epchc_station %in% unique(epcann$station)) %>% 
  group_by(yr, name, epchc_station) %>%
  summarise(val = mean(count, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    name = factor(name, levels = algnms, labels = algnms)
  ) %>% 
  rename(station = epchc_station, var = name) %>% 
  mutate(
    dat = 'alg',
    station = as.character(station)
  )

# SG coverage averaged to year
covann <- covest %>% 
  select(-foest) %>% 
  rename(var = Savspecies, station = Transect) %>% 
  mutate(yr = year(Date)) %>% 
  group_by(var, yr, station) %>% 
  summarise(val = mean(bbest, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    dat = 'cov',
    station = as.character(station)
  )

allann <- bind_rows(algann, covann, epcann)

save(allann, file = 'data/allann.RData', compress = 'xz')
