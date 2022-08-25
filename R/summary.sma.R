#' Print a summary
#' 
#' @description Writes a summary of a (standardized) major axis fit.
#' 
#' See \code{\link{sma}} for examples.
#' 
#' @param object Object of class 'sma'.
#' @param \dots Further arguments ignored.
#' @author Remko Duursma, Daniel Falster, David Warton
#' @seealso \code{\link{sma}},\code{\link{print.sma}}
#' @keywords misc
#' @export
summary.sma <- function(object,...){
	object$groupsummary
}

