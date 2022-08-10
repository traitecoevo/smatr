#' Extract coefficients from a 'sma' or 'ma' fit
#' 
#' Extracts elevation and slope of a standardized major axis (sma) or major
#' axis (ma) fit, for each of the groups if the fit was by group.
#' 
#' 
#' @param object Object of class 'sma'.
#' @param \dots Further arguments ignored.
#' @return A dataframe with the slope(s) and elevation(s), and their confidence
#' intervals. If the fit was by multiple groups, fits by all groups are
#' returned.
#' @author R.A. Duursma
#' @seealso \code{\link{sma}}
#' @keywords misc
coef.sma <- function(object, ...){

	x <- object
	if(length(x$coef) == 1){
		res <- x$coef[[1]][,1]
		names(res) <- c("elevation","slope")
	} else {
		res <- lapply(x$coef, "[", 1)
		res <- as.data.frame(do.call("rbind",lapply(res, t)))
		rownames(res) <- names(x$coef)
	}
	return(res)
	
}
