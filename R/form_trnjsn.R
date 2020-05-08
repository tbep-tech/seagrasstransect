#' Format training data from Water Atlas for plotting
#'
#' @param trnjsn output from \code{\link{read_trnjsn}}
#'
#' @return data frame in long format
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' trnjsn <- read_trnjsn()
#' form_trnjsn(trnjsn)
form_trnjsn <- function(trnjsn){
  
  out <- trnjsn %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(IDall = ID) %>% 
    tidyr::unnest('Observation') %>% 
    dplyr::select(Crew, Transect, Site, Savspecies = Species, Abundance = SpeciesAbundance, 
                  matches('BladeLength_|ShootDensity_')) %>% 
    dplyr::select(-BladeLength_Avg, -BladeLength_StdDev, -ShootDensity_Avg, -ShootDensity_StdDev) %>% 
    dplyr::mutate(
      Abundance = gsub('\\s=.*$', '', Abundance), 
      Abundance = as.numeric(Abundance)
    ) %>% 
    tidyr::gather('var', 'val', -Crew, -Transect, -Site, -Savspecies) %>% 
    dplyr::mutate(
      rep = gsub('.*([0-9])$', '\\1', var),
      rep = gsub('^Abundance$', '1', rep),
      var = gsub('[0-9]$', '', var),
      var = dplyr::case_when(
        var == 'BladeLength_' ~ 'Blade Length', 
        var == 'ShootDensity_' ~ 'Short Shoot Density', 
        T ~ var
      ), 
      val = gsub("[^0-9.-]", '', val), 
      val = as.numeric(val), 
      Site = as.character(Site)
    ) %>% 
    dplyr::group_by(Crew, Transect, Site, Savspecies, var) %>% 
    dplyr::summarise(
      aveval = mean(val, na.rm = T),
      sdval = sd(val, na.rm = T)
    ) %>% 
    dplyr::ungroup()
  
  return(out)
  
}