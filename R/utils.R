#' Utility plotting functions
#' 
#' @description Functions used in conjunction with \code{\link{plot.sma}} to customize
#' spacing of ticks on plot axes. \code{defineAxis} creates an 'axis' object,
#' including tick spacing, limits, and labels, that can be passed into
#' \code{\link{plot.sma}} or \code{nicePlot}. \code{nicePlot} creates an empty
#' plot using x and y axis objects.
#' 
#' @details By default, calls to \code{\link{plot.sma}} use the values given by
#' \code{\link{plot.default}} to determine axis limits and spacing of major and
#' minor ticks. Users can override these values by passing two 'axis' objects
#' created by \code{defineAxis} to \code{\link{plot.sma}}. When both x and y
#' axis objects are passed to \code{\link{plot.sma}}, the function uses
#' \code{nicePlot} to construct the axes according to the specified values,
#' instead of \code{\link{plot.default}}. As a minimum, users must at least one
#' argument (\code{major.ticks}) to \code{defineAxis} when passing these to
#' \code{\link{plot.sma}}.
#' 
#' @details The function \code{nicePlot} can also be called to construct a new set of
#' axes according to the specified values. For this to work, axis objects must
#' contain both \code{major.ticks} and \code{limits}.
#' 
#' @aliases plotutils defineAxis nicePlot
#' @param limits the x or y limits of the plot, (x1, x2) or (y1,y2).
#' @param major.ticks,minor.ticks Where to draw major and minor ticks
#' (vectors).
#' @param major.tick.labels Labels to draw at the ticks (optional).
#' @param both.sides a logical value indicating whether tickmarks should also
#' be drawn on opposite sides of the plot, i.e. right or top
#' @param xaxis,yaxis 'axis' objects, the result of calling 'defineAxis'.
#' @param ann a logical value indicating whether the default annotation (x and
#' y axis labels) should appear on the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param log One of 'x', 'y' or 'xy', specifying which axes draw in log10
#' scale.
#' @param frame.plot a logical indicating whether a box should be drawn around
#' the plot, by default = TRUE.
#' @param tck The length of tick marks as a fraction of the smaller of the
#' width or height of the plotting region. If tck >= 0.5 it is interpreted as a
#' fraction of the relevant side, so if tck = 1 grid lines are drawn. By
#' default set to current system defaults (tck = par("tck")).
#' @param \dots Arguments to be passed to nicePlot, and therein to 'axis'.
#' @seealso \code{\link{sma}}, \code{\link{plot.sma}}
#' @keywords misc
#' @noRd
#' @examples
#' \dontrun{
#' # Load leaf lifetime dataset:
#' data(leaflife)
#' 
#' #First example of axis spacing
#' ft <- sma(longev~lma*rain,log="xy",data=leaflife)
#' xax <- defineAxis(major.ticks=c(50, 100, 200, 400))
#' yax <- defineAxis(major.ticks=c(0.25, 0.5, 1,2,4,8))
#' plot(ft, xaxis=xax, yaxis=yax)
#' 
#' #As above, marking axes on both sides
#' xax <- defineAxis(major.ticks=c(50, 100, 200, 400), both.sides=TRUE)
#' yax <- defineAxis(major.ticks=c(0.25, 0.5, 1,2,4,8), both.sides=TRUE)
#' plot(ft, xaxis=xax, yaxis=yax)
#' 
#' #Using labels with log10 spacing and minor tick marks
#' xax <- defineAxis(limits=c(10, 1E3), major.ticks=seqLog(1, 1000), 
#' 	minor.ticks=makeLogMinor(seqLog(1, 1000)))
#' yax <- defineAxis(limits=c(1E-1, 1E1), major.ticks=seqLog(1E-2, 10), 
#' 	minor.ticks=makeLogMinor(seqLog(1E-2, 10)))
#' plot(ft, xaxis=xax, yaxis=yax)
#' 
#' #As above, but using expressions as labels
#' xax <- defineAxis(limits=c(10, 1E3), major.ticks=seqLog(10, 1000), 
#' 	minor.ticks=makeLogMinor(seqLog(10, 1000)), 
#' major.tick.labels = parse(text=paste("10^", c( 1,2,3), sep="")), 
#' 	both.sides=FALSE)
#' yax <- defineAxis(limits=c(1E-1, 1E1), major.ticks=seqLog(1E-1, 10), 
#' 	minor.ticks=makeLogMinor(seqLog(1E-1, 10)), 
#' major.tick.labels = parse(text=paste("10^", c( -1,0,1), sep="")), 
#' 	both.sides=FALSE)
#' plot(ft, xaxis=xax, yaxis=yax)
#' 
#' #start an empty plot using nicePlot 
#' xax <- defineAxis(limits=c(8, 1.2E3), major.ticks=seqLog(1, 1000))
#' yax <- defineAxis(limits=c(0.8E-1, 1.2E1), major.ticks=seqLog(1E-2, 10))
#' nicePlot(xax,yax,log='xy')
#' }



