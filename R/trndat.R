#' Imported training data
#'
#' @format A data frame
#' @examples
#' \dontrun{
#' pth <- 'inst/extdata/Class_Data_20190627.xlsx'
#' trndat <- read_traindat(pth)
#' save(trndat, file = here::here('data', 'trndat.RData'), compress = 'xz')
#' }
"trndat"