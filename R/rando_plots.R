library(tbeptools)
library(tidyverse)
library(patchwork)
library(lubridate)

source('R/funcs.R')

data(algdat)
data(allann)

# for factor levels
algnms <- c('Bacillariophyta', 'Cyanobacteria', 'Karenia brevis', 'Pseudo-nitzschia pungens', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense', 'Tripos hircus', 'other')
cols <- pal_alg(algnms)
names(cols) <- algnms
savnms <- c('AA', 'DA', 'Halodule', 'Thalassia', 'Syringodium', 'Ruppia', 'Halophila spp.')

# names for map comparison selection
names(algnms) <- algnms
wqalgnms <- c(list('Total nitrogen' = 'tn', 'Chorophyll-a' = 'chla', 'Secchi' = 'sd_raw_m', 'Salinity' = 'Sal_Mid_ppth', 'Temperature' = 'Temp_Water_Mid_degC'), as.list(algnms))


# habs and wq over time ---------------------------------------------------

toplo <- allann %>% 
  filter(yr >= 2009 & yr <= 2019) %>%
  group_by(var, yr, dat) %>% 
  nest %>% 
  mutate(
    data = purrr::map(data, function(x){
      
      ave <- try({mean(x$val, na.rm = TRUE)})
      brs <- try({t.test(x$val)$conf.int %>% diff() / 2})
      
      if(inherits(ave, 'try-error'))
        ave <- NA
      
      if(inherits(brs, 'try-error'))
        brs <- NA
      
      sumout <- data.frame(ave = ave, brs = brs)
      
      return(sumout)
      
    })
  ) %>% 
  unnest('data') %>% 
  ungroup() %>% 
  mutate(
    var = factor(var, levels = c(unlist(wqalgnms), savnms), labels = c(names(wqalgnms), savnms))
  ) %>% 
  filter(var %in% c('Pseudo-nitzschia sp.',  'Pyrodinium bahamense', 'Temperature', 'Salinity')) %>% 
  mutate(
    var = factor(var, 
      levels = c('Salinity', 'Temperature', 'Pseudo-nitzschia sp.',  'Pyrodinium bahamense'), 
      labels = c('Salinity (psu)', 'Temp (C)', 'Pseudo-nitzschia sp. (0.1/mL)',  'Pyrodinium bahamense (0.1/mL)')
      )
    )

p <- ggplot(toplo, aes(x = yr, y = ave)) + 
  geom_errorbar(aes(ymin = ave -brs, ymax = ave + brs, colour = dat), width = 0) + 
  geom_point(size = 2) +
  facet_wrap(~var, ncol = 4, strip.position = 'left', scales = 'free_y') + 
  theme_minimal(base_size = 12) +
  scale_x_continuous(breaks = c(2010, 2015)) + 
  scale_colour_manual(values = c('darkorange', 'light blue')) +
  # stat_smooth(method = 'lm') + 
  theme(
    strip.background = element_blank(), 
    axis.title = element_blank(), 
    strip.placement = 'outside', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = 'none'
  ) 
png('~/Desktop/pyropseu.png', height = 3, width = 9, units = 'in',res = 200)
print(p)  
dev.off()


# bivariate ----------------------------------------------------------------

sz <- 7
szy <- 9
xvr <- 'Salinity (psu)'
yvr <- 'Pseudo-nitzschia sp. (0.1/mL)'
toplo1 <- toplo %>%  
  filter(var %in% c(xvr, yvr)) %>% 
  gather('sum', 'val', ave, brs) %>% 
  unite('varsum', dat, sum, sep = '_') %>% 
  select(-var) %>%
  spread(varsum, val)


p1 <- ggplot(toplo1, aes(x = epc_ave, y = alg_ave)) + 
  geom_errorbar(aes(ymin = alg_ave - alg_brs, ymax = alg_ave + alg_brs), colour = 'darkorange', width = 0) + 
  geom_errorbarh(aes(xmin = epc_ave - epc_brs, xmax = epc_ave + epc_brs), colour = 'lightblue',height = 0) + 
  geom_point(size = 2) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(size = sz),
    axis.title = element_text(size = szy),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = xvr, 
    y = yvr
  )

xvr <- 'Salinity (psu)'
yvr <- 'Pyrodinium bahamense (0.1/mL)'
toplo1 <- toplo %>%  
  filter(var %in% c(xvr, yvr)) %>% 
  gather('sum', 'val', ave, brs) %>% 
  unite('varsum', dat, sum, sep = '_') %>% 
  select(-var) %>%
  spread(varsum, val)

p2 <- ggplot(toplo1, aes(x = epc_ave, y = alg_ave)) + 
  geom_errorbar(aes(ymin = alg_ave - alg_brs, ymax = alg_ave + alg_brs), colour = 'darkorange', width = 0) + 
  geom_errorbarh(aes(xmin = epc_ave - epc_brs, xmax = epc_ave + epc_brs), colour = 'lightblue',height = 0) + 
  geom_point(size = 2) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(size = sz),
    axis.title = element_text(size = szy),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = xvr, 
    y = yvr
  )



xvr <- 'Temp (C)'
yvr <- 'Pseudo-nitzschia sp. (0.1/mL)'
toplo1 <- toplo %>%  
  filter(var %in% c(xvr, yvr)) %>% 
  gather('sum', 'val', ave, brs) %>% 
  unite('varsum', dat, sum, sep = '_') %>% 
  select(-var) %>%
  spread(varsum, val)

p3 <- ggplot(toplo1, aes(x = epc_ave, y = alg_ave)) + 
  geom_errorbar(aes(ymin = alg_ave - alg_brs, ymax = alg_ave + alg_brs), colour = 'darkorange', width = 0) + 
  geom_errorbarh(aes(xmin = epc_ave - epc_brs, xmax = epc_ave + epc_brs), colour = 'lightblue',height = 0) + 
  geom_point(size = 2) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(size = sz),
    axis.title = element_text(size = szy),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = xvr, 
    y = yvr
  )

xvr <- 'Temp (C)'
yvr <- 'Pyrodinium bahamense (0.1/mL)'
toplo1 <- toplo %>%  
  filter(var %in% c(xvr, yvr)) %>% 
  gather('sum', 'val', ave, brs) %>% 
  unite('varsum', dat, sum, sep = '_') %>% 
  select(-var) %>%
  spread(varsum, val)

p4 <- ggplot(toplo1, aes(x = epc_ave, y = alg_ave)) + 
  geom_errorbar(aes(ymin = alg_ave - alg_brs, ymax = alg_ave + alg_brs), colour = 'darkorange', width = 0) + 
  geom_errorbarh(aes(xmin = epc_ave - epc_brs, xmax = epc_ave + epc_brs), colour = 'lightblue',height = 0) + 
  geom_point(size = 2) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size = sz),
    axis.title = element_text(size = szy),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = xvr, 
    y = yvr
  )

pout <- p1 + p2 + p3 + p4 + plot_layout(ncol = 4)

png('~/Desktop/pyropseubv.png', height = 3, width = 9, units = 'in',res = 200)
print(pout)  
dev.off()


# phyto seasonal ----------------------------------------------------------

toplo1 <- algdat %>% 
  filter(yr >= 1998 & yr <= 2006) %>%
  filter(epchc_station %in% allann$station) %>% 
  filter(name %in% c('Bacillariophyta', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense')) %>% 
  group_by(mo, name) %>% 
  summarise(
    count = sum(count, na.rm = T)
  ) %>% 
  ungroup %>% 
  complete(mo, name, fill = list(count = 0)) %>% 
  mutate(
    name = factor(name, levels = c('Bacillariophyta', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense'))
  )

cols <- c('mediumseagreen', 'darkolivegreen1', 'gold2')
names(cols) <- c('Bacillariophyta', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense')

p1 <- ggplot(toplo1, aes(x = as.numeric(mo), y = count, fill = name)) + 
  geom_area(position = 'fill', alpha = 0.7) + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 12), labels = levels(algdat$mo)) + 
  scale_y_continuous(expand = c(0, 0)) + #, limits = c(0, 140)) + 
  scale_fill_manual(values = cols, drop = F) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.title = element_blank(), 
    legend.position = 'none'
  ) + 
  labs(
    y = "Propportion cell count", 
    title = c('1998 - 2006')
  )

toplo2 <- algdat %>% 
  filter(yr > 2006) %>% 
  filter(epchc_station %in% allann$station) %>% 
  filter(name %in% c('Bacillariophyta', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense')) %>% 
  group_by(mo, name) %>% 
  summarise(
    count = sum(count, na.rm = T)
  ) %>% 
  ungroup %>% 
  complete(mo, name, fill = list(count = 0))  %>% 
  mutate(
    name = factor(name, levels = c('Bacillariophyta', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense'))
  )

p2 <- ggplot(toplo2, aes(x = as.numeric(mo), y = count, fill = name)) + 
  geom_area(position = 'fill', alpha = 0.7) + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 12), labels = levels(algdat$mo)) + 
  scale_y_continuous(expand = c(0, 0)) + #, limits = c(0, 140)) + 
  scale_fill_manual(values = cols) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.title = element_blank()#, 
    # legend.position = 'top'
  ) + 
  labs(
    y = "Proportion cell count (0.1/mL)", 
    title = c('2009 - 2019')
  )

out <- (p1 + plot_layout(ncol = 2, guides = 'collect') & theme(legend.position = 'bottom')) + (p2 + theme(legend.position = 'none'))

png('~/Desktop/phytoseas.png', height = 4.5, width = 10, units = 'in',res = 200)
print(out)  
dev.off()

# salinity by segment -----------------------------------------------------

epcann <- epcdata %>% 
  filter(yr >= 2009) %>% 
  filter(yr <= 2019) %>% 
  select(bay_segment, epchc_station, SampleTime, Sal_Bottom_ppth, Sal_Mid_ppth, Sal_Top_ppth, Temp_Water_Bottom_degC, Temp_Water_Mid_degC, Temp_Water_Top_degC) %>% 
  gather('var', 'val', -bay_segment, -epchc_station, -SampleTime) %>% 
  mutate(
    SampleTime = as.Date(SampleTime), 
    var = gsub('\\_ppth$|\\_degC$', '', var), 
    var = gsub('^Temp\\_Water', 'TempWater', var)
  ) %>% 
  separate(var, c('var', 'dep'), sep = '_') %>% 
  group_by(bay_segment, epchc_station, SampleTime, var) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  mutate(
    yr = year(SampleTime)
  ) %>% 
  group_by(bay_segment, epchc_station, yr, var) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  group_by(bay_segment, yr, var) %>% 
  nest %>% 
  mutate(
    res = purrr::map(data, function(x){
      
      avev <- try({mean(x$val, na.rm = T)})
      barv <- try({diff(t.test(x$val, na.rm = T)$conf.int) / 2})
      
      if(inherits(avev, 'try-error'))
        avev <- NA
      if(inherits(barv, 'try-error'))
        barv <- NA
      
      out <- data.frame(avev = avev, barv = barv)
      
      return(out)
      
    })
  ) %>% 
  select(-data) %>% 
  unnest(res) %>% 
  gather('stat', 'val', avev, barv) %>% 
  unite(var, stat, 'var') %>% 
  spread(var, val) %>% 
  mutate(bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB')))

toplo <- epcann

p <- ggplot(toplo, aes(x = yr, y = avev_Sal)) + 
  geom_errorbar(aes(ymin = avev_Sal - barv_Sal, ymax = avev_Sal + barv_Sal), colour = 'lightblue', width = 0) + 
  geom_point(size = 2) +
  facet_wrap(~bay_segment, ncol = 4) + 
  theme_minimal(base_size = 12) +
  scale_x_continuous(breaks = c(2010, 2015)) + 
  # stat_smooth(method = 'lm') + 
  theme(
    strip.background = element_blank(),
    strip.placement = 'outside', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = 'none', 
    axis.title.x = element_blank()
  )  + 
  labs(
    y = 'Salinity (psu)'
  )

png('~/Desktop/allsal.png', height = 3, width = 9, units = 'in',res = 200)
print(p)  
dev.off()

p <- ggplot(toplo, aes(x = yr, y = avev_TempWater)) + 
  geom_errorbar(aes(ymin = avev_TempWater - barv_TempWater, ymax = avev_TempWater + barv_TempWater), colour = 'lightblue', width = 0) + 
  geom_point(size = 2) +
  facet_wrap(~bay_segment, ncol = 4) + 
  theme_minimal(base_size = 12) +
  scale_x_continuous(breaks = c(2010, 2015)) + 
  # stat_smooth(method = 'lm') + 
  theme(
    strip.background = element_blank(),
    strip.placement = 'outside', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = 'none', 
    axis.title.x = element_blank()
  )  + 
  labs(
    y = 'Temp (C)'
  )

png('~/Desktop/alltmp.png', height = 3, width = 9, units = 'in',res = 200)
print(p)  
dev.off()
