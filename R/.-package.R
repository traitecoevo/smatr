

#' Leaf longevity and leaf mass per area for plant species from different sites
#' 
#' This dataset contains the leaf longevity and leaf mass per area of 75 plant
#' species from four sites in south-eastern Australia. Sites represent
#' contrasts along rainfall and soil phosphorous gradients.
#' 
#' 
#' @name leaflife
#' @docType data
#' @format A data frame containing 75 rows and 5 variables: \describe{
#' \item{site}{the site at which this species was sampled (coded as 1, 2, 3, or
#' 4)}
#' 
#' \item{rain}{The level of annual rainfall at the site (coded as "high" or
#' "low")}
#' 
#' \item{soilp}{The level of soil phosphate concentration at the site (coded as
#' "high" or "low")}
#' 
#' \item{longev}{Leaf longevity (years)}
#' 
#' \item{lma}{Leaf mass per area (m2/kg)} }
#' @source Wright I. J., Westoby M. and Reich P. B. (2002) Convergence towards
#' higher leaf mass per area in dry and nutrient-poor habitats has different
#' consequences for leaf life span.  \emph{Journal of Ecology} \bold{90},
#' 534--543.
#' @keywords datasets
NULL





#' Leaf mass per area and photosynthetic rate for plant species from different
#' sites
#' 
#' This dataset contains the leaf mass per area (LMA) and photosynthetic rate
#' per meaf mass (Amass) of individual plants from 81 plant species at four
#' sites in south-eastern Australia. Sites represent contrasts along rainfall
#' and soil phosphorous gradients.
#' 
#' 
#' @name leafmeas
#' @docType data
#' @format A data frame containing 529 rows and 4 variables: \describe{
#' \item{site}{(factor) The site at which this species was sampled (coded as
#' Murrua, WH, RHM, RHW)}
#' 
#' \item{spp}{(factor) Species of the observed plant species}
#' 
#' \item{Amass}{(numeric) Photosynthetic rate per leaf mass ()}
#' 
#' \item{lma}{(numeric) Leaf mass per area (m2/kg)} }
#' @source Wright I. J., Reich P. B. and Westoby M. (2001) Strategy shifts in
#' leaf physiology, structure and nutrient content between species of high- and
#' low-rainfall and high- and low-nutrient habitats.  \emph{Functional Ecology}
#' \bold{15}, 423--434.
#' @keywords datasets
NULL





#' Utility plotting functions
#' 
#' Functions used in conjunction with \code{\link{plot.sma}} to customize
#' spacing of ticks on plot axes. \code{defineAxis} creates an 'axis' object,
#' including tick spacing, limits, and labels, that can be passed into
#' \code{\link{plot.sma}} or \code{nicePlot}. \code{nicePlot} creates an empty
#' plot using x and y axis objects.
#' 
#' By default, calls to \code{\link{plot.sma}} use the values given by
#' \code{\link{plot.default}} to determine axis limits and spacing of major and
#' minor ticks. Users can override these values by passing two 'axis' objects
#' created by \code{defineAxis} to \code{\link{plot.sma}}. When both x and y
#' axis objects are passed to \code{\link{plot.sma}}, the function uses
#' \code{nicePlot} to construct the axes according to the specified values,
#' instead of \code{\link{plot.default}}. As a minimum, users must at least one
#' argument (\code{major.ticks}) to \code{defineAxis} when passing these to
#' \code{\link{plot.sma}}.
#' 
#' The function \code{nicePlot} can also be called to construct a new set of
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
#' @examples
#' 
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
#' 
#' 
NULL





#' (Standardised) Major Axis Estimation and Testing Routines
#' 
#' This package provides methods of fitting bivariate lines in allometry using
#' the major axis (MA) or standardised major axis (SMA), and for making
#' inferences about such lines. The available methods of inference include
#' confidence intervals and one-sample tests for slope and elevation, testing
#' for a common slope or elevation amongst several allometric lines,
#' constructing a confidence interval for a common slope or elevation, and
#' testing for no shift along a common axis, amongst several samples.
#' 
#' The key functions in this package are \code{\link{sma}} and
#' \code{\link{ma}}, which will fit SMA and MA respectively, and construct
#' confidence intervals or test hypotheses about slope or elevation in one or
#' several samples, depending on how the arguments are used.
#' 
#' For example:
#' 
#' \code{sma(y~x)} will fit a SMA for \code{y} vs \code{x}, and report
#' confidence intervals for the slope and elevation.
#' 
#' \code{sma(y~x, robust=T)} will fit a robust SMA for \code{y} vs \code{x}
#' using Huber's M estimation, and will report (approximate) confidence
#' intervals for the slope and elevation.
#' 
#' \code{ma(y~x*groups-1)} will fit MA lines for \code{y} vs \code{x} that are
#' forced through the origin, where a separate MA is fitted to each of several
#' samples as specified by the argument \code{groups}. It will also report
#' results from a test of the hypothesis that the true MA slope is equal across
#' all samples.
#' 
#' For more details, see the help listings for \code{\link{sma}} and
#' \code{\link{ma}}.
#' 
#' Note that the \code{\link{sma}} and \code{\link{ma}} functions replace the
#' functions given in earlier package versions as \code{\link{line.cis}},
#' \code{\link{slope.test}}, \code{\link{elev.test}}, \code{\link{slope.com}},
#' \code{\link{elev.com}} and \code{\link{shift.com}}, although all of these
#' functions and their help entries are still available.
#' 
#' All procedures have the option of correcting for measurement error, although
#' only in an approximate fashion, valid in large samples.
#' 
#' Additional features of this package are listed below.
#' 
#' \describe{ \item{meas.est}{ Estimates the average variance matrix of
#' measurement error for a set of subjects with repeated measures } }
#' 
#' \bold{Example datasets:} \describe{ \item{leaflife}{ leaf longevity and leaf
#' mass per area for plant species from different sites. Used to demonstrate
#' the functionality of the \code{sma} and \code{ma} functions.}
#' \item{leafmeas}{ leaf mass per area and photosynthetic rate for plant
#' species from different sites. Used to demonstrate the meas.est function } }
#' 
#' For more details, see the documentation for any of the individual functions
#' listed above.
#' 
#' @docType package
#' @author Warton, D. \email{David.Warton@@unsw.edu.au}, Duursma, R., Falster,
#' D. and Taskinen, S.
#' @seealso \code{\link{sma}},\code{\link{ma}}, \code{\link{meas.est}},
#' \code{\link{leaflife}}, \code{\link{leafmeas}}
#' @references Warton D. I. and Weber N. C. (2002) Common slope tests for
#' bivariate structural relationships.  \emph{Biometrical Journal} \bold{44},
#' 161--174.
#' 
#' Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of
#' bivariate line-fitting methods for allometry.  \emph{Biological Reviews}
#' \bold{81}, 259--291.
#' 
#' Taskinen S. and Warton D. I. (in press) Robust estimation and inference for
#' bivariate line-fitting in allometry.  \emph{Biometrical Journal}.
#' @keywords documentation
#' @examples
#' 
#' 
#' # See  ?sma and ?plot.sma for a full list of examples.
#' 
#' 
NULL



