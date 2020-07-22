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

save(otbtrn, file = here::here('data', 'otbtrn.RData'), compress = 'xz')

# algae data --------------------------------------------------------------

# file path
xlsx <- '~/Desktop/phyto_data.xlsx'

# load and assign to object
algdat <- read_importphyto(xlsx, download_latest = T) %>%
  filter(yr < 2020)

save(algdat, file = here::here('data', 'algdat.RData'), compress = 'xz')

