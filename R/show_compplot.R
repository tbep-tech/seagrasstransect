#' Make a bar plot for training group comparisons
#'
#' @param dat data frame returned by \code{\link{form_traindat}}
#' @param site numeric value indicating site results to plot
#' @param species chr string indicating which species to plot
#' @param varplo chr string indicating which variable to plot 
#' @param base_size numeric indicating text scaling size for plot
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @export
#'
#' @examples
#' dat <- form_traindat(trndat)
#' show_compplot(dat)
show_compplot <- function(dat, site = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'), species = c('Halodule', 'Thalassia'), 
                          varplo = c('Abundance', 'Blade Length', 'Short Shoot Density'), base_size = 18){

  # arguments
  site <- match.arg(site)
  species <- match.arg(species)
  varplo <- match.arg(varplo)
  
  # labels
  lbs <- c('Braun-Blanquet Score', 'Canopy Height (+/- 1 sd)', 'Short Shoot Counts (+/- 1 sd)')
  names(lbs) <- c('Abundance', 'Blade Length', 'Short Shoot Density')
  xlb <- lbs[[varplo]]
  ttl <- paste('Site', site)
  sublbs <- c('Halodule wrightii', 'Thalassia testidinum')
  names(sublbs) <- c('Halodule', 'Thalassia')
  subttl <- sublbs[[species]]
  
  # data to plot
  toplo <- dat %>% 
    dplyr::filter(Site %in% site) %>% 
    dplyr::filter(Savspecies %in% species) %>% 
    dplyr::filter(var %in% varplo) %>% 
    dplyr::mutate(Agency = factor(Agency, levels = rev(unique(Agency))))
  
  # plot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = aveval, y = Agency)) + 
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