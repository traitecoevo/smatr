#' Sequence Generation
#' 
#' @description Generate multiplicative sequences, or series.
#' 
#' @details Starting at \code{from}, \code{seq} multiplies successively by \code{base}
#' until the maximal value is reached. This is useful for generating
#' tick-spacing on log-transformed axes.
#' 
#' @param from,to the starting and (maximal) end value of a sequence.
#' @param base multiplication value.
#' @keywords internal
#' @returns vector from lo to hi with multiplication steps of incr. Used for making ticks to a log-scaled axis 
#' @examples
#' \dontrun{
#' #Sequence suitable for log base 10 labels
#' seqLog(1E-5, 1E5)
#' 
#' #Sequence suitable for log base 2 labels
#' seqLog(2, 128,base=2)
#' }
seqLog <- function(from, to, base=10){base^(log(from,base):log(to,base))}
