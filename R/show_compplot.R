#' Make a bar plot for training group comparisons
#'
#' @param dat data frame returned by \code{\link{form_trndat}}
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
#' show_compplot(dat, site = '1', species = 'Halodule', varplo = 'Abundance')
show_compplot <- function(dat, site, species = c('Halodule', 'Ruppia', 'Syringodium', 'Thalassia'), 
                          varplo = c('Abundance', 'Blade Length', 'Short Shoot Density'), base_size = 18){

  # arguments
  species <- match.arg(species)
  varplo <- match.arg(varplo)
  
  # labels
  lbs <- c('Braun-Blanquet Score', 'Canopy Height (+/- 1 sd)', 'Short Shoot Counts (+/- 1 sd)')
  names(lbs) <- c('Abundance', 'Blade Length', 'Short Shoot Density')
  xlb <- lbs[[varplo]]
  ttl <- paste('Site', site)
  sublbs <- c('Halodule wrightii', 'Ruppia maritima', 'Syringodium filiforme', 'Thalassia testidinum')
  names(sublbs) <- c('Halodule', 'Ruppia', 'Syringodium', 'Thalassia')
  subttl <- sublbs[[species]]
  
  # data to plot
  toplo <- dat %>% 
    dplyr::filter(Site %in% site) %>% 
    dplyr::filter(Savspecies %in% species) %>% 
    dplyr::filter(var %in% varplo) %>% 
    dplyr::mutate(
      Crew = gsub('(.{1,12})(\\s|$)', '\\1\n', Crew)
      )

  # plot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = Crew, y = aveval)) + 
    ggplot2::geom_bar(stat = 'identity', alpha = 0.7) + 
    ggplot2::labs(
      y = xlb, 
      title = ttl, 
      subtitle = bquote(italic(.(subttl)))
    ) + 
    # ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2)) + 
    ggplot2::scale_y_continuous(expand = c(0, 0)) + 
    ggplot2::theme_bw(base_size = base_size) + 
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(), 
      axis.text.x = ggplot2::element_text(size = 8),
      panel.grid.major.x = ggplot2::element_blank(), 
      panel.grid.minor.x = ggplot2::element_blank()
    )
    
  if(!varplo %in% 'Abundance') 
    p <- p + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = aveval - sdval, ymax = aveval + sdval), width = 0.25)
  
  return(p)

}