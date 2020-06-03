#' Import JSON transect data from Water Atlas
#'
#' @param url chr string of API endpoint
#'
#' @return data frame
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' 
#' # get training data by default
#' trnjsn <- read_trnjsn()
#' 
#' # import all transect data
#' url <- 'http://dev.seagrass.wateratlas.usf.edu/api/assessments/all__use-with-care'
#' trnjsn <- read_trnjsn(url)
read_trnjsn <- function(url = 'http://dev.seagrass.wateratlas.usf.edu/api/assessments/training'){
  
  dat <- jsonlite::fromJSON(url)
  
  return(dat)
  
}