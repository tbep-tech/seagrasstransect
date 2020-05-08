#' Make a bar plot for training group comparisons
#'
#' @param dat data frame returned by \code{\link{form_trndat}}
#' @param transect chr string indicating transect results to plot
#' @param site chr string indicating site results to plot
#' @param species chr string indicating which species to plot
#' @param varplo chr string indicating which variable to plot 
#' @param base_size numeric indicating text scaling size for plot
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @examples
#' dat <- form_trndat(trndat)
#' show_compplot(dat, transect = 'TRAINING01', site = '1', species = 'Halodule', varplo = 'Abundance')
show_compplot <- function(dat, transect, site, species = c('Halodule', 'Ruppia', 'Syringodium', 'Thalassia'), 
                          varplo = c('Abundance', 'Blade Length', 'Short Shoot Density'), base_size = 18){

  # arguments
  species <- match.arg(species)
  varplo <- match.arg(varplo)
  
  # labels
  lbs <- c('Braun-Blanquet Score', 'Canopy Height (+/- 1 sd)', 'Short Shoot Counts (+/- 1 sd)')
  names(lbs) <- c('Abundance', 'Blade Length', 'Short Shoot Density')
  xlb <- lbs[[varplo]]
  ttl <- paste0('Transect ', transect, ', Site ', site)
  sublbs <- c('Halodule wrightii', 'Ruppia maritima', 'Syringodium filiforme', 'Thalassia testidinum')
  names(sublbs) <- c('Halodule', 'Ruppia', 'Syringodium', 'Thalassia')
  subttl <- sublbs[[species]]
  
  # data to plot
  toplo <- dat %>% 
    dplyr::filter(Transect %in% transect) %>% 
    dplyr::filter(Site %in% site) %>% 
    dplyr::filter(Savspecies %in% species) %>% 
    dplyr::filter(var %in% varplo) %>% 
    dplyr::mutate(Crew = factor(Crew, levels = rev(unique(Crew))))
  
  # plot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = aveval, y = Crew)) + 
    ggplot2::geom_bar(stat = 'identity', alpha = 0.7) + 
    ggplot2::labs(
      x = xlb, 
      title = ttl, 
      subtitle = bquote(italic(.(subttl)))
    ) + 
    ggplot2::scale_x_continuous(expand = c(0, 0)) + 
    ggplot2::theme_bw(base_size = base_size) + 
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(), 
      panel.grid.major.y = ggplot2::element_blank(), 
      panel.grid.minor.y = ggplot2::element_blank()
    )
    
  if(!varplo %in% 'Abundance') 
    p <- p + 
      ggplot2::geom_errorbar(ggplot2::aes(xmin = aveval - sdval, xmax = aveval + sdval), width = 0.25)
  
  return(p)

}