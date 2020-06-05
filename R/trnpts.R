#' Seagrass transect starting locations
#'
#' Seagrass transect starting locations
#'
#' @format A \code{sf} POINT object
#' @examples 
#' \dontrun{
#' library(sf)
#' 
#' trnpts <- st_read('T:/05_GIS/SEAGRASS_TRANSECTS/TransectBasics2019.shp') %>% 
#'    st_transform(crs = 4326)
#' 
#' save(trnpts, file = 'data/trnpts.RData', compress = 'xz')
#' }
"trnpts"