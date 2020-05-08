#' Import training data from file
#'
#' @param pth chr string for location of file
#' @param sheet chr string of sheet to import
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' pth <- 'inst/extdata/Class_Data_20190627.xlsx'
#' read_trndat(pth)
#' }
read_trndat <- function(pth, sheet = 'All data'){
  
  dat <- readxl::read_excel(pth, sheet = sheet)
  
  return(dat)
  
}