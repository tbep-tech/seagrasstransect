
# content for water atlas seagrass transect web page ---------------------

library(tbeptools)
library(dplyr)
library(tidyr)
library(extrafont)
library(flextable)
library(rmarkdown)

loadfonts(device = 'pdf', quiet = T)
if(Sys.info()[1] == 'Windows')
  loadfonts(device = 'win', quiet = T)

# check if data are current -----------------------------------------------

newdat <- read_transect()

if(file.exists('data/trndat.RData')){
  
  load(file = 'data/trndat.RData')
  
  iscurrent <- identical(newdat, trndat)

  if(!iscurrent)
    save(trndat, file = 'data/trndat.RData', compress = 'xz')
    
} else {
  
  trndat <- newdat
  iscurrent <- FALSE
  
  save(trndat, file = 'data/trndat.RData', compress = 'xz')
  
}

# create new files if data aren't current ---------------------------------

if(!iscurrent){
    
  transectocc <- anlz_transectocc(trndat)
  
  # report card graphic -----------------------------------------------------
  
  p <- show_transectmatrix(transectocc, family = 'Lato')
  
  jpeg('docs/reportcard.jpg', height = 8, width = 4, units = 'in', res = 300, family = 'Lato')
  print(p)
  dev.off()
  
  # frequency occurrence graphic --------------------------------------------
  
  p <- show_transectavespp(transectocc, family = 'Lato')
  
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
    dplyr::select(`Bay segment` = bay_segment, Year = yr, `# of quadrats` = nsites, everything())
  
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
  
  write.csv(trndat, 'docs/trantab.csv', row.names = F)
  write.csv(transectocc, 'docs/tranocctab.csv', row.names = F)
  
  # metadata ----------------------------------------------------------------
  
  render('docs/metadata.md', html_document())

  # render README for date update -------------------------------------------
  
  render('README.Rmd')
  
}