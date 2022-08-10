#' Print a summary
#' 
#' Writes a summary of a (standardized) major axis fit.
#' 
#' Does not add much to print.sma, at the moment, except when fit by multiple
#' groups. See \code{\link{sma}} for examples.
#' 
#' @param object Object of class 'sma'.
#' @param \dots Further arguments ignored.
#' @author Remko Duursma, Daniel Falster, David Warton
#' @seealso \code{\link{sma}},\code{\link{print.sma}}
#' @keywords misc
summary.sma <- function(object,...){
	object$groupsummary
}

