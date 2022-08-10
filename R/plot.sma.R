#' Draw an X-Y plot
#' 
#' @description Plot a (standardized) major axis fit, including the data and the fitted
#' lines. There are many options for changing the appearance of the plot and
#' generating high-quality publishable graphs.
#' 
#' @details The \code{plot.sma} function produces one of three different types of plot,
#' depending on the \code{which} argument.
#' 
#' The default plot, \code{which="default"}, produced a plot of \code{y} vs
#' \code{x}, with separate symbols for each \code{group} if appropriate, and MA
#' or SMA lines fitted through each group. The formula used in the \code{sma}
#' object that is input to the \code{plot} function determines whether there is
#' a group structure, whether fitted lines have common slope, etc.
#' 
#' A residual plot can be obtained via \code{which="residual"} - this is a plot
#' of residuals against fitted values. This can be used to check assumptions -
#' if assumptions are satisfied there should be no pattern.
#' 
#' A normal quantile plot can be obtained via \code{which="qq"} - this is a
#' normal quantile plot of residuals. This can be used to check the normality
#' assumption - if data are close to a straight line, normality is plausible.
#' Note that non-normality is only important to the validity of the test in
#' small samples. In larger samples, non-normality will not effect validity,
#' but strong non-normality will reduce the power of tests.
#' 
#' If \code{use.null=TRUE} then the lines added to the plot use the
#' coefficients estimated under the null hypothesis. For example, if the sma
#' object \code{x} was produced using a common slopes test (via
#' \code{y~x*groups} or similar) then \code{use.null=TRUE} will plot lines that
#' apply the common slope to each group.
#' 
#' The arguments \code{pch}, \code{col}, \code{lty}, \code{from & to}, are used
#' to modify characteristics of the plotted points and lines. If a vector of
#' values for anyone of these arguments is passed to \code{plot.sma}, then
#' successive values are applied to each group, provided group structure is
#' included in \code{x} and the vector length is at least as long as the number
#' of groups.
#' 
#' By default, \code{plot.sma} uses the default tick spacing given by
#' \code{plot.default}. To customise axes, users can pass special axis objects
#' to \code{plot.sma}, obtained using the \code{\link{defineAxis}} command as
#' in the example below. This enables high quality publishable plots to be
#' produced. See \code{\link{plotutils}} for more information.
#' 
#' @param x Object of class 'sma'.
#' @param which If 'residual', plots a residual plot; if 'qq', plots a qq plot;
#' otherwise an x-y plot.
#' @param use.null Logical. If FALSE, plots the fitted lines (the default),
#' otherwise those corresponding to the null hypothesis.
#' @param add Logical. If TRUE, adds lines or points to an existing plot.
#' @param type As in 'lm.default' : 'p' plots points only, 'l' only lines, and
#' 'o' or 'b' plot both.
#' @param xaxis,yaxis Special axis objects. See Details and examples.
#' @param xlab,ylab Labels for X and Y axes.
#' @param pch Plotting symbols (see \code{\link{points}}).
#' @param col Color of points and lines.
#' @param lty Line type (see \code{\link{lines}}).
#' @param from,to Min and max X values for the lines (defaults to values given
#' by \code{\link{sma}}, which are the X values corresponding the maximum and
#' minimum fitted values in the data.).
#' @param log One of 'x','y' or 'xy', to denote which axes to log-scale.
#' @param frame.plot a logical indicating whether a box should be drawn around
#' the plot, by default = TRUE.
#' @param tck The length of tick marks as a fraction of the smaller of the
#' width or height of the plotting region. If tck >= 0.5 it is interpreted as a
#' fraction of the relevant side, so if tck = 1 grid lines are drawn. By
#' default set to current system defaults (tck = par("tck")).
#' @param p.lines.transparent Adjusts transparency level of fitted lines
#' according to p-value of correlation between X and Y, via formula opacity =
#' 1-p/p.lines.transparent). Setting to a value 0.1 means the line for any
#' group with p = 0.1 would be fully transparent, while line for a group with p
#' =0.05 would be 50 perecent transparent. by default set to NA, which means
#' lines are fully visible.
#' @param axes If FALSE, suppress plotting of the axes (Default TRUE)
#' @param \dots Further arguments passed to \code{\link{plot.default}}.
#' @author D. Falster, R.A. Duursma, D.I. Warton
#' @seealso \code{\link{sma}}, \code{\link{plotutils}},
#' \code{\link{defineAxis}}
#' @keywords misc
#' @export
#' @examples
#' 
#' # Load leaf lifetime dataset:
#' data(leaflife)
#' 
#' # Only consider low-nutrient sites:
#' leaf.low.soilp <- subset(leaflife, soilp == 'low')
#' 
#' # Fit SMA's separately at each of high and low 
#' # rainfall sites and test for common slope:
#' ft <- sma(longev~lma*rain, data=leaf.low.soilp, log="xy")
#' 
#' # Plot leaf longevity (longev) vs leaf mass per area (lma) 
#' # separately for each of high and low rainfall:
#' plot(ft)
#' 
#' # As before but add lines which have a common slope:
#' plot(ft, use.null=TRUE)
#' 
#' #As above, but adding the common slope lines to an existing plot
#' plot(ft, type='p', col="black")
#' plot(ft, use.null=TRUE, add=TRUE, type='l')
#' 
#' # Plot with equally spaced tick marks:
#' plot(ft, xaxis=defineAxis(major.ticks=c(40,80,160,320,640)), 
#' 	yaxis=defineAxis(major.ticks=c(0.5,1,2,4,8)) )
#' 
#' # Produce a residual plot to check assumptions:
#' plot(ft,which="res")
#' 
#' # Produce a normal quantile plot:
#' plot(ft,which="qq")
#' 
#' 
plot.sma <- function(x, which=c("default","residual","qq"),  use.null=FALSE, add=FALSE, type='o', 
	xaxis=NULL, yaxis=NULL, xlab=NULL, ylab=NULL, pch=NULL, col=NULL, lty=NULL, from=NULL, to = NULL, log=x$log, 
	frame.plot = TRUE, tck=par("tck"),p.lines.transparent=NA, axes=TRUE, ...){

	# function used to make colours transparent alpha = 0 means fully transparaent
	make.transparent <- function(col, alpha=1) {
  		tmp <- col2rgb(col)/255
	rgb(tmp[1,], tmp[2,], tmp[3,], alpha=alpha)
	}

	#preprocessing ------------------------------------------------------------
	obj <- x  # this needed for consistency with plot
	if(obj$gt == "none"){
		ngrps <- 1	
	}
	else{
		groups <- levels(obj$data[,3])
		ngrps <- length(groups)
	}
	
	whichplot <- match.arg(which)
	
	#---colors--------------------------------
	#user-defined colors
	if(!is.null(col[1])){	
		if(length(col)== 1 &&  ngrps > 1)
			col<-rep(col[1],ngrps); #check right vector length 
	} else {
	#default colors
		col <- c("blue2",  "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
		"darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3",
		"goldenrod4", "darkolivegreen2", "palevioletred3", "darkseagreen3", "sandybrown", "tan", 
		"gold", "violetred4", "darkgreen")
		col <- rep(col, ceiling(ngrps/length(col)))
	}
	
	#---symbols--------------------------------	
	#user-defined symbols
	if(!is.null(pch[1])){	
		if(length(pch) == 1 && ngrps > 1)
			pch <- rep(pch[1],ngrps) #check right vector length 
	} else #default SYMBOLS		
		pch <- rep(1,ngrps) 
	#---line type--------------------------------	#user-defined symbol	
	if(!is.null(lty[1])){
			if(length(lty) == 1 && ngrps > 1)
			lty<-rep(lty[1],ngrps) #check right vector length 
	} else #default SYMBOLS
		lty <- rep("solid", ngrps)
	#-----------------------------------------------------------------------------
	# DATA PLOT	
	if(whichplot == "default"){

		 #obtain data--------------------------------
		 Y <- obj$data[,1] 
		 X <- obj$data[,2] 
	    
	    #log trasnformations--------------------------------
	    log_data <- obj$log	#logging of data based on transformation applied in sma. Allows scaling of axes ot be changed, while maintaing correct transformation of data
		XLog <- YLog <- 0
		if((log_data == "x") | (log_data == "xy")){ XLog=1; X = 10^X}
		if((log_data == "y") | (log_data == "xy")){ YLog=1; Y = 10^Y}

		#axis labels--------------------------------
	    #determine axis labels if not already specified
	    if(is.null(xlab)){
	    	xlab <- names(obj$data)[2]
			if(XLog)
				xlab <- paste(xlab, "[log scale]")
		}
	    if(is.null(ylab)){
	    	ylab <- names(obj$data)[1]
			if(YLog)
				ylab <- paste(ylab, "[log scale]")
		}

		# SETUP AXES--------------------------------
		if(add==FALSE) {
			
      # Use nice plotting if appropriate
			if(!is.null(xaxis)  && !is.null(yaxis)){
				
				# Deteremine axis limits if missing - caluclated on transformed data. 				
        # add 5% white space around plots, then back transform if necessary
				if (is.null(xaxis$limits)){
					Range_x <-range(obj$data[,2])
					Buffer <- (Range_x[2]-Range_x[1])*0.05
					xaxis$limits <- c(Range_x[1]-Buffer, Range_x[2]+Buffer) 
					if(XLog)xaxis$limits <- 10^xaxis$limits
				}
        
				if (is.null(yaxis$limits)){
					Range_y <-range(obj$data[,1])
					#add 4% white space around plots (like R default in plot)
					Buffer <- (Range_y[2]-Range_y[1])*0.04
					yaxis$limits <- c(Range_y[1]-Buffer, Range_y[2]+Buffer)   
					if(YLog) yaxis$limits <- 10^yaxis$limits
				}
				
				# Make plot
				nicePlot(xaxis, yaxis, log=log, xlab=xlab, ylab=ylab, 
					frame.plot = frame.plot, tck=tck, ...)
			}
			else
				plot(X, Y, type='n', log=log, xlab=xlab, ylab=ylab,
					frame.plot = frame.plot, tck=tck, axes=axes, ...)
		}
	
		#add datapoints	--------------------------------
		if(type %in% c("o","b", "p")){
			if(obj$gt == "none")
				points(X, Y, col = col[1], pch=pch[1], ...)
			else{
				for(i in 1:ngrps){
					iref  <- as.character(obj$data[,3]) == groups[i]
					points(X[iref], Y[iref], col =col[i], pch=pch[i],...)
				}
			}
		}
		
		#add lines --------------------------------
		if(type %in% c("o","b", "l")){
			
			#decide which coefficients to use: alternative (default) or null
			if(use.null==FALSE)
				coef <- obj$coef
			else
				coef <- obj$nullcoef
			
			#determine end points for lines
			if(is.null(from[1])){  #based on fitted values
				for(i in 1:ngrps){
					from[i] <- as.numeric(obj$from[i])  
					to[i] <- as.numeric(obj$to[i])
				}
			} else {  #user defined
				if(length(from) == 1){
					from <- rep(from[1], ngrps)
					to <- rep(to[1], ngrps)
				}
			}
			
			# Add lines to plot
			for(i in 1:ngrps){
				
        # Coefficients
				a <- coef[[i]][1,1]
				B <- coef[[i]][2,1]
				
			  p <- obj$groupsummary$pval[i]

				if(!is.na(p.lines.transparent))
					col.tr <- make.transparent(col[i], max(0, (1 - p/p.lines.transparent)))
				else
					col.tr <-  col[i]

				#choose line according to log-trsnaformation used in fitting data, even if different transformation used for axes
				if(log_data=="xy")
	        		curve(10^a*x^B, from[i], to[i], add=TRUE,col = col.tr, lty= lty[i],...)
    	   	 	if(log_data=="x")
        			curve(a+B*log10(x), from[i], to[i], add=TRUE, col = col.tr, lty= lty[i],...)
        		if(log_data=="y")
        			curve(10^(a+x*B), from[i], to[i], add=TRUE, col = col.tr, lty= lty[i],...)
       	    	if(log_data=="")
        			curve(a + x*B, from[i], to[i], add=TRUE,  col = col.tr, lty= lty[i],...)
        	}
        }
	}

	# RESIDUAL PLOT	
	if(whichplot == "residual")
	{
		 #obtain data--------------------------------
		Y <- fitted.sma(obj, type = "residuals")
		X <- fitted.sma(obj, type = "fitted")

		
		#axis labels--------------------------------
	    #determine axis labels if not already specified
	    if(is.null(xlab)) xlab <- paste("Fitted values (",names(obj$data)[2], " v ",
			names(obj$data)[1],")")  
	    if(is.null(ylab)) ylab <- paste("Residual values (",names(obj$data)[2], " v ",
			names(obj$data)[1],")")  

		#SETUP AXES--------------------------------
		if(!add){ #use default plotting options 
			plot(X,Y, type='n', xlab=xlab, ylab=ylab, frame.plot = frame.plot,...)
   		}
		
		#add datapoints	--------------------------------
		if(type %in% c("o","b", "p")){
			if(obj$gt == "none")
				points(X, Y, col = col[1], pch=pch[1],...)
			else{
				for(i in 1:ngrps){
					iref <- as.character(obj$data[,3]) == groups[i]
					points(X[iref], Y[iref], col =col[i], pch=pch[i],...)
				}
			}
		}
	}

	# QQ PLOT	
	if(whichplot == "qq")
	{
		 #obtain data--------------------------------
		Y <- fitted.sma(obj, type = "residuals")
		
		#axis labels--------------------------------
	    #determine axis labels if not already specified
	    if(is.null(xlab)) xlab <- "Normal quantiles"
	    if(is.null(ylab)) ylab <- paste("Residual values (",names(obj$data)[2], " v ",
			names(obj$data)[1],")")  

		#SETUP AXES--------------------------------
		if(add==FALSE){ #use default plotting options 
			qqnorm(Y, xlab=xlab, ylab=ylab,...)
			qqline(Y)
   		}
	}

}
	
	
