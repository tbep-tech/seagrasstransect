#' Format training data for plotting
#'
#' @param trndat output from \code{\link{read_traindat}}
#'
#' @return data frame in long format
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' form_traindat(trndat)
form_traindat <- function(trndat){
  
  out <- trndat %>% 
    dplyr::select(Agency, Site, Savspecies, Abundance, matches('^BL|^SSD')) %>% 
    dplyr::filter(Savspecies %in% c('Halodule', 'Thalassia')) %>% 
    tidyr::gather('var', 'val', -Agency, -Site, -Savspecies) %>% 
    dplyr::mutate(
      rep = gsub('.*([0-9])$', '\\1', var),
      rep = gsub('^Abundance$', '1', rep),
      var = gsub('[0-9]$', '', var),
      var = dplyr::case_when(
        var == 'BL' ~ 'Blade Length', 
        var == 'SSD' ~ 'Short Shoot Density', 
        T ~ var
      ), 
      val = gsub("[^0-9.-]", '', val), 
      val = as.numeric(val)
    ) %>% 
    dplyr::group_by(Agency, Site, Savspecies, var) %>% 
    dplyr::summarise(
      aveval = mean(val, na.rm = T),
      sdval = sd(val, na.rm = T)
    )
  
  return(out)
  
}