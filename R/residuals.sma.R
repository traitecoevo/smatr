#' Extract model residuals
#' 
#' Extracts the residuals of a (standardized) major axis fit.
#' 
#' Residuals are calculated as y-bx-a for each group. These values are useful
#' in assumption checking, especially in constructing residual vs fitted value
#' plots.
#' 
#' @param object Object of class 'sma'.
#' @param \dots Further arguments ignored.
#' @return A vector of residuals.
#' @seealso \code{\link{sma}}, \code{\link{plot.sma}}
#' @keywords misc
residuals.sma <- function(object, ...){
	return(fitted(object, type = "residuals",...))
}
