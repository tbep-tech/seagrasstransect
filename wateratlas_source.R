
# content for water atlas seagrass transect web page ---------------------

library(tbeptools)
library(tidyverse)
library(extrafont)
library(flextable)

loadfonts(device = 'pdf', quiet = T)
if(Sys.info()[1] == 'Windows')
  loadfonts(device = 'win', quiet = T)

dat <- read_transect()
transectocc <- anlz_transectocc(dat)

# report card graphic -----------------------------------------------------

p <- show_transectmatrix(transectocc)

jpeg('docs/reportcard.jpg', height = 8, width = 5, units = 'in', res = 300, family = 'Lato')
print(p)
dev.off()

# frequency occurrence graphic --------------------------------------------

p <- show_transectavespp(transectocc)

jpeg('docs/freqocc.jpg', height = 5, width = 8, units = 'in', res = 300, family = 'Lato')
print(p)
dev.off()

# tabular summary ---------------------------------------------------------

# annual average by segment
totab <- transectocc %>% 
  anlz_transectavespp( 
    total = T, 
    bay_segment = c('HB', 'OTB', 'MTB', 'LTB', 'BCB'), 
    yrrng = c(1998, 2020), 
    species = c('Halodule', 'Syringodium', 'Thalassia', 'Halophila', 'Ruppia', 'Caulerpa'), 
    by_seg = T
    ) %>%
  dplyr::mutate(
    foest = 100 * foest, 
    foest = round(foest, 1)
    ) %>%
  tidyr::spread(Savspecies, foest) %>%
  dplyr::arrange(bay_segment, yr) %>% 
  select(`Bay segment` = bay_segment, Year = yr, `# of quadrats` = nsites, everything())

tab <- totab %>% 
  flextable %>% 
  set_formatter(
    `No Cover` = function(x) sprintf("%.01f", x),
    `Halodule` = function(x) sprintf("%.01f", x),
    `Syringodium` = function(x) sprintf("%.01f", x),
    `Thalassia` = function(x) sprintf("%.01f", x),
    `Halophila` = function(x) sprintf('%.01f', x),
    `Ruppia` = function(x) sprintf("%.01f", x),
    `Caulerpa` = function(x) sprintf("%.01f", x)
  ) %>% 
  merge_v(j = 'Bay segment') %>%
  valign(j = 'Bay segment', valign = 'top') %>%
  padding(padding = 0, part = 'all') %>%
  fontsize(size = 12, part = 'all') 

save_as_html(tab, 'docs/freqocctab.html')

# data download -----------------------------------------------------------

write.csv(dat, 'docs/trantab.csv', row.names = F)
write.csv(transectocc, 'docs/tranocctab.csv', row.names = F)

# metadata ----------------------------------------------------------------

__Complete transect data__: Minimally altered raw transect data for every transect and site (quadrat) along each transect.  

* *Date*: Sample date in YYYY-MM-DD
* *Transect*: Transect name
* *Site*: Quadrat location along each transect, meters
* *Depth*: Site depth, meters. Should be negative values below the water surface.
* *Savspecies*: Relevant seagrass genus, also includes AA (attached algae, multiple types including Caulerpa), DA (drift algae), and no cover
* *SeagrassEdge*: Depth of seagrass edge, cm
* *var*: Measured variable, one of abundance, blade length, or short shoot density
* *aveval*: Average value for the measured variable (multiple quadrats at a site depending on variable), unitless if abundance, cm if blade length, shoots per square meter if short shoot density
* *sdval*: Standard deviation for the measured variable, same units as in aveval

__Summarized frequency occurrence data__: Summarized data at each transect and date, aggregating information across sites (quadrats).

* *Date*: Sample date in YYYY-MM-DD
* *Transect*: Transect name
* *Savspecies*: Relevant seagrass genus, also includes AA (attached algae), DA (drift algae), and no cover
* *nsites*: Number of sites (quadrats) at each transect/date, repeated across rows  
* *foest*: Frequency occurrence estimate for the whole transect for the species, from 0 - 1
* *bbest*: Abundance (Braun-Blaunquet) average across sites (quadrats) for the species, from 0 - 5

