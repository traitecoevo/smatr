#' Generate spacing for minor tick marks on a plot with log-scaled axes
#' 
#' @description Generates a sequence of numbers providing minor tick marks on log scaled
#' axes.
#' 
#' A vector is created according to the following algorithm. For each pair of
#' adjacent values (x1, x2) in \code{major}, the function adds the values (x1,
#' 2*x1, 3*x1, ..., x2) to the vector of return values.
#' 
#' This is useful for generating spacing of minor tick-values on
#' log-transformed axes.
#' 
#' @param major a vector of values giving major tick marks.
#' @keywords internal
#' @return vector of minor tick spacings approrpaite for log 10 scaled axis with major ticks given by 'major'
#' @examples
#' \dontrun{
#' #Sequence suitable for log base 10 labels
#' makeLogMinor(seqLog(1E-5, 1E5))
#' }
makeLogMinor<-function(major){	
	temp <- NULL
	if(length(major) > 1) 
		for (i in 1:(length(major)-1))
			temp<-c(temp, seq(major[i], major[i+1]-major[i], major[i]))
	
	temp <- c(temp, major[length(major)])
	return(temp)
} 
