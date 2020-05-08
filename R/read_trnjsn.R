#' Import JSON training data from Water Atlas
#'
#' @param url chr string of API endpoint
#'
#' @return data frame
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' trnjsn <- read_trnjsn()
read_trnjsn <- function(url = 'http://dev.seagrass.wateratlas.usf.edu/api/assessments/training'){
  
  dat <- jsonlite::fromJSON(url)
  
  return(dat)
  
}