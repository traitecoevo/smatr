#' Major axis estimation and testing for one or several samples
#'
#' @param ... arguments passed to sma()
#'
#' @export

ma <- function(...){
  sma(..., method="MA")
  }