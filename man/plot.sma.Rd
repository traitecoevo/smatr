\name{plot.sma}
\alias{plot.sma}
\title{Draw an X-Y plot	}
\description{Plot a (standardized) major axis fit, including the data and the fitted lines. There are many options for 
changing the appearance of the plot and generating high-quality publishable graphs.
}
\usage{
\method{plot}{sma}(x, which = c("default", "residual", "qq"), 
	use.null = FALSE, add = FALSE, 	type = "o", xaxis = NULL, 
	yaxis = NULL, xlab = NULL, ylab = NULL, pch = NULL, 
	col = NULL, lty = NULL, from = NULL, to = NULL, 
	log = x$log, frame.plot = TRUE, tck=par("tck"),...)}
	
\arguments{
  \item{x}{Object of class 'sma'.}
  \item{which}{If 'residual', plots a residual plot; if 'qq', plots a qq plot; otherwise an x-y plot.}
  \item{use.null}{Logical. If FALSE, plots the fitted lines (the default), otherwise those corresponding to the null hypothesis.}
  \item{add}{Logical. If TRUE, adds lines or points to an existing plot.}
  \item{type}{As in 'lm.default' : 'p' plots points only, 'l' only lines, and 'o' or 'b' plot both.}
  \item{xaxis,yaxis}{Special axis objects. See Details and examples.}
  \item{xlab,ylab}{Labels for X and Y axes.}
  \item{pch}{Plotting symbols (see \code{\link{points}}).}
  \item{col}{Color of points and lines.}
  \item{lty}{Line type (see \code{\link{lines}}).}
  \item{from,to}{Min and max X values for the lines (defaults to values given by \code{\link{sma}}, which are the  
X values corresponding the maximum and minimum fitted values in the data.).}
  \item{log}{One of 'x','y' or 'xy', to denote which axes to log-scale.}
  \item{frame.plot}{a logical indicating whether a box should be drawn around the plot, by default = TRUE. }
  \item{tck}{The length of tick marks as a fraction of the smaller of the width or height of the plotting region. If tck >= 0.5 it is interpreted as a fraction of the relevant side, so if tck = 1 grid lines are drawn. By default set to current system defaults (tck = par("tck")).}

  \item{\dots}{Further arguments passed to \code{\link{plot.default}}.}
}
\details{
The \code{plot.sma} function produces one of three different types of plot, depending on the \code{which} argument.

The default plot, \code{which="default"}, produced a plot of \code{y} vs \code{x}, with separate symbols for each \code{group} if appropriate, and MA or SMA lines fitted through each group. The formula used in the \code{sma} object that is input to the \code{plot} function determines whether there is a group structure, whether fitted lines have common slope, etc.

A residual plot can be obtained via \code{which="residual"} - this is a plot of residuals against fitted values. This can be used to check assumptions - if assumptions are satisfied there should be no pattern.

A normal quantile plot can be obtained via \code{which="qq"} - this is a normal quantile plot of residuals. This can be used to check the normality assumption - if data are close to a straight line, normality is plausible. Note that non-normality is only important to the validity of the test in small samples. In larger samples, non-normality will not effect validity, but strong non-normality will reduce the power of tests.

If \code{use.null=TRUE} then the lines added to the plot use the coefficients estimated under the null hypothesis. For example, if the sma object \code{x} was produced using a common slopes test (via \code{y~x*groups} or similar) then \code{use.null=TRUE} will plot lines that apply the common slope to each group.

The arguments \code{pch},  \code{col},  \code{lty},  \code{from & to}, are used to modify characteristics of the plotted points and lines. If a vector of values for anyone of these arguments is passed to \code{plot.sma}, then successive values are applied to each group, provided group structure is included in \code{x} and the vector length is at least as long as the number of groups. 

By default, \code{plot.sma} uses the default tick spacing given by \code{plot.default}. To customise axes, users can pass special axis objects to \code{plot.sma}, obtained using the \code{\link{defineAxis}} command as in the example below. This enables high quality publishable plots to be produced. See \code{\link{plotutils}} for more information.
}

\examples{
# Load leaf lifetime dataset:
data(leaflife)

# Only consider low-nutrient sites:
leaf.low.soilp <- subset(leaflife, soilp == 'low')

# Fit SMA's separately at each of high and low 
# rainfall sites and test for common slope:
ft <- sma(longev~lma*rain, data=leaf.low.soilp, log="xy")

# Plot leaf longevity (longev) vs leaf mass per area (lma) 
# separately for each of high and low rainfall:
plot(ft)

# As before but add lines which have a common slope:
plot(ft, use.null=TRUE)

#As above, but adding the common slope lines to an existing plot
plot(ft, type='p', col="black")
plot(ft, use.null=TRUE, add=TRUE, type='l')

# Plot with equally spaced tick marks:
plot(ft, xaxis=defineAxis(major.ticks=c(40,80,160,320,640)), 
	yaxis=defineAxis(major.ticks=c(0.5,1,2,4,8)) )

# Produce a residual plot to check assumptions:
plot(ft,which="res")

# Produce a normal quantile plot:
plot(ft,which="qq")

}

\author{ D. Falster, R.A. Duursma, D.I. Warton }
\seealso{
\code{\link{sma}}, \code{\link{plotutils}}, \code{\link{defineAxis}}
}

\keyword{misc}
