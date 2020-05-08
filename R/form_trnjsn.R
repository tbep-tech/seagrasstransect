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
    tibble::as_tibble()
  
  return(out)
  
}