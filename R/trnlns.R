#' Seagrass transect locations
#'
#' Seagrass transect locations
#'
#' @format A \code{sf} LINESTRING object
#' @examples 
#' \dontrun{
#' library(sf)
#' 
#' trnlns <- st_read('T:/05_GIS/SEAGRASS_TRANSECTS/transect_routes.shp') %>% 
#'    st_transform(crs = 4326)
#' 
#' save(trnlns, file = 'data/trnlns.RData', compress = 'xz')
#' }
"trnlns"