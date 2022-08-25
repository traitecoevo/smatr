#' (Standardized) major axis estimation and testing for one or several samples
#' 
#' @description The \code{sma} and \code{ma} functions fit SMA and MA respectively, and
#' construct confidence intervals or test hypotheses about slope or elevation
#' in one or several samples, depending on how the arguments are specified.
#' Options exist to force lines through the origin, (approximately) correct for
#' measurement error if the measurement error variance is known, and use robust
#' estimation to ensure that inferences are valid in the presence of outliers
#' (currently implemented for single samples only).
#' 
#' @details This is the key function in the \code{\link{smatr-package}}; all the key
#' estimation and testing functionality in the package can all be accessed
#' using this function, via different usages of the \code{formula} and other
#' arguments, as described below.
#' 
#' \bold{One-sample testing} The below options allow estimation of a (S)MA,
#' confidence intervals for parameters, and hypothesis testing of parameters,
#' from a single sample of two variables \code{y} and \code{x}. Use the
#' \code{sma} function to fit a standardised major axis (SMA), or use \code{ma}
#' in combination with the below options in order to fit major axis (MA)
#' instead. \describe{ \item{list("sma(y~x)")}{ Fits a SMA and constructs
#' confidence intervals for the true slope and elevation. Replaces the
#' \code{\link{line.cis}} function from previous versions of smatr. }
#' \item{list("ma(y~x)")}{ Fits a MA and constructs confidence intervals for
#' the true slope and elevation. All the below functions also work for MA, if
#' the \code{ma} function is called instead of the \code{sma} function.}
#' \item{list("sma(y~x, slope.test=B)")}{ Tests if the slope of a SMA equals
#' \code{B}. } \item{list("sma(y~x, elev.test=A)")}{ Tests if the elevation of
#' a SMA equals \code{A}. } \item{list("sma(y~x, robust=T)")}{ Fits a SMA using
#' Huber's M estimation and constructs confidence intervals for the true slope
#' and elevation. This offers robustness to outliers in estimation and
#' inference, and can be used in combination with the \code{slope.test} and
#' \code{elev.test} arguments. } \item{list("sma(y~x-1)")}{ Fits a SMA where
#' the line is forced through the origin, and constructs confidence intervals
#' for the true slope. This type of formula can be used in combination with the
#' \code{slope.test} argument.} }
#' 
#' \bold{For several samples:} The below options allow estimation of several
#' (S)MA lines, confidence intervals for parameters, and hypothesis testing of
#' parameters, from two variables \code{y} and \code{x} for observations that
#' have been classified into several different samples using the factor
#' \code{groups}. Use the \code{sma} function to fit a standardised major axis
#' (SMA), or use the \code{ma} in combination with the below options in order
#' to fir major axis (MA) instead.  \describe{ \item{list("sma(y~x*groups)")}{
#' Test if several SMA lines share a common slope, and construct a confidence
#' interval for the true common slope. } \item{list("sma(y~x+groups,
#' type="elevation")")}{ Test if several common slope SMA lines also share a
#' common elevation, and construct a confidence interval for the true common
#' elevation. } \item{list("sma(y~x+groups, type="shift")")}{ Test if several
#' groups of observations have no shift in location along common slope SMA
#' lines. } \item{list("sma(y~x*groups, slope.test=B)")}{ Test if several SMA
#' lines share a common slope whose true value is exactly equal to \code{B}. }
#' \item{list("sma(y~x+groups, elev.test=A)")}{ Test if several common-slope
#' SMA lines share a common elevation whose true value is exactly equal to
#' \code{A}. } \item{list("sma(y~x*groups-1)")}{ Test if several SMA lines
#' forced through the origin share a common slope. This can also be used in
#' combination with the \code{slope.test} argument or when testing for no shift
#' along common (S)MA lines.} }
#' 
#' In all cases, estimates and confidence intervals for key parameters are
#' returned, and if a hypothesis test is done, results will be returned and
#' stored in the \code{slope.test} or \code{elev.test} output arguments.
#' 
#' The \code{\link{plot}} function can be applied to objects produced using the
#' \code{sma} and \code{ma} functions, which is highly recommended to visualise
#' results and check assumptions.
#' 
#' \bold{Multiple comparisons} If multcomp=TRUE, pair-wise comparisons are made
#' between levels of the grouping variable for differences in slope, elevation
#' or shift, depending on the formula used. The P values can be adjusted for
#' multiple comparisons (using the Sidak correction). See also
#' \code{\link{multcompmatrix}} for visualization of the results.
#' \bold{Warning:} When using the multiple comparisons (multcomp=TRUE), you
#' must specify a \code{data} statement. If your variables are not in a
#' dataframe, simply combine them in a dataframe before calling \code{sma}.
#' 
#' @param formula Formula of the form y ~ x etc. This determines whether a
#' single (S)MA is fitted, or whether several lines are fitted and compared.
#' See Details.
#' @param data A dataframe with the x and y variables.
#' @param subset A subset of the dataframe for fitting; optional.
#' @param na.action What to do with missing values (na.omit, na.fail, etc).
#' @param log One of 'x', 'y', or 'xy' to log10-transform variables.
#' @param method If SMA, standardized major axis, if MA, major axis, and if
#' OLS, ordinary least squares.
#' @param type If several lines with common slope are to be compared, do you
#' want to test for a change in 'elevation' or for a 'shift' along a common
#' (S)MA. See Details.
#' @param alpha The error rate for confidence intervals. Typically 0.05.
#' @param slope.test The hypothesised value to be used, if testing for evidence
#' that (S)MA slope(s) are significantly different from a hypothesised value.
#' @param elev.test The hypothesised value to be used, if testing for evidence
#' that (S)MA elevation(s) are significantly different from a hypothesised
#' value.
#' @param robust If TRUE, uses a new method of robust line fitting. (Currently
#' available for single-sample testing only.)
#' @param V The estimated variance matrix of measurement error. Average
#' measurement error for Y is in the first row and column, and average
#' measurement error for X is in the second row and column. The default is that
#' there is no measurement error.
#' @param n_min The minimum sample size for a group.
#' @param quiet If TRUE, suppresses all messages.
#' @param multcomp Logical. If TRUE, performs pair-wise comparisons between
#' levels of the grouping variable.
#' @param multcompmethod Whether to adjust the P values for multiple
#' comparisons ('adjusted') or not ('default').
#' @param \dots Further arguments passed to internal functions (none at the
#' moment?)
#' @return An object of class \code{sma} or \code{ma}, which contains the
#' following output arguments: \describe{ \item{coef}{ The coefficients of the
#' fitted (standardised) major axes. If several samples are being compared,
#' this will return parameters from the alternative model, e.g. assuming
#' separate slopes if testing for common slope, or assuming common slope but
#' separate elevations if testing for common elevation. } \item{nullcoef}{The
#' coefficients under the null hypothesis} \item{alpha}{ As above. }
#' \item{method}{ The method used in fitting lines: 'MA' or 'SMA' }
#' \item{intercept}{ Whether or not (S)MA lines were forced through the origin
#' (True or False). } \item{call}{ The call to the \code{ma} or \code{sma}
#' function. } \item{data}{ As above. } \item{log}{ As above. }
#' \item{variables}{ A list of the variables used in fitting (S)MA lines. }
#' \item{origvariables}{ A list of the variables prior to transformation (if
#' any) for use in fitting (S)MA lines. } \item{groups}{Levels of the grouping
#' variable, if present in the fit.} \item{gt}{Type of grouptest
#' ("slopecom","elevcom",or "shiftcom"), if it was carried out, or "none" if
#' none.} \item{gtr}{The result of that grouptest.} \item{slopetest}{ Output
#' from the hypothesis test of slope(s), if any. Returned as a list of objects,
#' including the P-value (\code{p}), the test statistic (\code{r} or
#' \code{LR}), the (common) slope (\code{b}) and its confidence interval
#' (\code{ci}). } \item{elevtest}{ Output from the hypothesis test of
#' elevation(s), if any. Returned as a list of objects, including the P-value
#' (\code{p}), the test statistic (\code{t} or \code{stat}), the (common)
#' elevation (\code{a}) and its confidence interval (\code{ci}). }
#' \item{slopetestdone}{Whether a slopetest was actually carried out.}
#' \item{elevtestdone}{Whether an elevation test was actually carried out.}
#' \item{n}{Sample size(s).} \item{r2}{Squared correlation coefficient.}
#' \item{pval}{P-value of the test of correlation coefficient against zero.}
#' \item{from,to}{X values corresponding the maximum and minimum fitted values
#' in each group. Used by plot.sma to determine endpoints for fitted lines).}
#' \item{groupsummary}{Neatly organized dataframe with coefficients by group.}
#' }
#' @author Warton, D. I. \email{David.Warton@@unsw.edu.au}, R.A. Duursma, D.
#' Falster, S. Taskinen
#' @export
#' @seealso \code{\link{plot.sma}}
#' @references Warton, D.I., R.A. Duursma, D.S. Falster and S. Taskinen (2012).
#' smatr 3 - an R package for estimation and inference about allometric lines.
#' \emph{Methods in Ecology and Evolution}. \bold{3}, 257-259.
#' 
#' Warton D. I. and Weber N. C. (2002) Common slope tests for bivariate
#' structural relationships.  \emph{Biometrical Journal} \bold{44}, 161--174.
#' 
#' Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of
#' bivariate line-fitting methods for allometry.  \emph{Biological Reviews}
#' \bold{81}, 259--291.
#' 
#' Taskinen S. and Warton D. I. (in press) Robust estimation and inference for
#' bivariate line-fitting in allometry.  \emph{Biometrical Journal}.
#' @keywords misc
#' @examples
#' 
#' 
#' # Load leaf lifetime dataset:
#' data(leaflife)
#' 
#' ### One sample analyses ###
#' # Extract only low-nutrient, low-rainfall data:
#' leaf.low <- subset(leaflife, soilp == 'low' & rain == 'low')
#' 
#' # Fit a MA for log(leaf longevity) vs log(leaf mass per area):
#' ma(longev ~ lma, log='xy', data=leaflife)
#' 
#' # Test if the MA slope is not significantly different from 1:
#' ma.test <- ma(longev ~ lma, log='xy', slope.test=1, data=leaflife)
#' summary(ma.test)
#' 
#' # Construct a residual plot to check assumptions:
#' plot(ma.test,type="residual")
#' 
#' ### Several sample analyses ###
#' 
#' # Now consider low-nutrient sites (high and low rainfall):
#' leaf.low.soilp <- subset(leaflife, soilp == 'low')
#' 
#' # Fit SMA's separately at each of high and low rainfall sites,
#' # and test for common slope:
#' com.test <- sma(longev~lma*rain, log="xy", data=leaf.low.soilp)
#' com.test
#' 
#' # Plot longevity vs LMA separately for each group:
#' plot(com.test)
#' 
#' # Fit SMA's separately at each of high and low rainfall sites,
#' # and test if there is a common slope equal to 1:
#' sma(longev~lma*rain, log="xy", slope.test=1, data=leaf.low.soilp)
#' 
#' # Fit SMA's with common slope across each of high and low rainfall sites, 
#' # and test for common elevation:
#' sma(longev~lma+rain, log="xy", data=leaf.low.soilp)
#' 
#' # Fit SMA's with common slope across each of high and low rainfall sites, 
#' # and test for no shift along common SMA:
#' sma(longev~lma+rain, log="xy", type="shift", data=leaf.low.soilp)
#' 
#' 
sma <- function(formula, data, subset, na.action, log='',
	 method=c("SMA","MA","OLS"), type=c("elevation","shift"), alpha=0.05, 
	 slope.test=NA, elev.test=NA, multcomp=FALSE, multcompmethod=c("default","adjusted"),
	 robust=FALSE,V=matrix(0,2,2),n_min=3,quiet=FALSE,
	 ...)
{
 
  method <- match.arg(method)
	type <- match.arg(type)
	multcompmethod <- match.arg(multcompmethod)
	
	# Model frame (code borrowed from 'lm')
	mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
	mf <- eval(mf, parent.frame())
	if(ncol(mf) == 3)mf[,3] <- as.factor(mf[,3])
	
	# Throw out groups with <3 observations, if a grouping variable is used.
	# (Otherwise, there are in total 2 rows of data, surely no user is that daft).
	if(ncol(mf) == 3){
	tab <- table(mf[,3])
	if(any(tab < n_min)){
		
		thislevel <- levels(mf[,3])[which(tab < n_min)]
		mf <- mf[!(mf[,3] %in% thislevel),]
		mf <- droplevels(mf)
		attributes(mf)$row.names <- rownames(mf)
		if(!quiet){
      message("Warning: dropped level of grouping variable (sample size < ",n_min,") :")
      message(paste(names(mf)[3]," = ",thislevel))
	  }
	}
	}
	
	# Log-transform 
	log <- tolower(log)
	
	# Throw out zeroes and negative values.
	xdel <- FALSE
	ydel <- FALSE
	if(log == "xy" || log == "y"){
		if(any(mf[,1] <= 0)){
			ind <- which(mf[,1] <= 0)
			ydel <- TRUE
			mf <- droplevels(mf[-ind,])
		}
	}
	if(log == "xy" || log == "x"){
		if(any(mf[,2] <= 0)){
			ind <- which(mf[,2] <= 0)
			xdel <- TRUE
			mf <- droplevels(mf[-ind,])
		}
	}
	
	if(log == "x")mf[,2] <- log10(mf[,2])
	if(log == "y")mf[,1] <- log10(mf[,1])
	if(log == "xy"){
		mf[,2] <- log10(mf[,2])
		mf[,1] <- log10(mf[,1])
	}
	if(!(log %in% c("","x","y","xy")))
		warning("Log transformation ignored! Use one of '', 'x','y' or 'xy'")
	
	# Prepare testing by group.
	mt <- attr(mf, "terms")
	vn <- names(mf)  # variable names
	tn <- attr(mt, "term.labels")   # term names
	
	# Check if there is group testing, and check that formula is compatible.
	if(length(tn) == 1)grouptest <- "none"
	if(length(tn) == 2){
		formulacheck <- all(tn == c(vn[2], vn[3]))
		if(formulacheck){
			if(type == "elevation")grouptest <- "elevcom"
			if(type == "shift")grouptest <- "shiftcom"
		} else grouptest <- "malformed"
	}
	# Slope test implied by formula (z+y+z:y = z*y)
	if(length(tn) == 3){
		formulacheck <- all(tn == c(vn[2], vn[3], paste(vn[2],":",vn[3],sep="")))
		if(formulacheck)grouptest <- "slopecom" else grouptest <- "malformed"
	}
	if(length(tn) > 3 || grouptest == "malformed"){
		warning("Formula not supported by sma() and/or ma(), and is ignored.")
		grouptest <- "none"
	}

	# Check for intercept. Also note that it is not allowed to drop the intercept for some group tests,
	# but this is not yet tested here!
	intercept <- if(attr(mt, "intercept") == 0) FALSE else TRUE
	
	# Halt execution when robust=T and no intercept.
	if(robust & !intercept)stop("Cannot perform robust estimation when no intercept included.")
	
	#Determine grouping 
	if(grouptest %in% c("elevcom","shiftcom","slopecom")){
		ngroups <- nlevels(mf[,3])
		
		# Fix the V matrix, when multiple grouping (cf. email david on Feb 4)
		if (length(dim(V)) == 2 & nrow(V) == 2 & ncol(V) == 2)
		  V2 <- array(V, c(dim(V), ngroups))
		else
		  V2=V
		
		grps<-mf[,3]
		lv <- levels(grps)
		
		
		if(method == "OLS"){
			commonslopetest <- NA
			commonslopetestval <- NA
			grouptestresult <- ""
		} else {
  		

  		# Whenever there are groups, do test for common slope.
  		commonslopetest <- slope.com(mf[,1], mf[,2], mf[,3], alpha=alpha, 
                                   intercept=intercept, method=method, group.names=lv, V=V2, robust=robust)
  		
  		# Test the common slope against hypthesized value, if this option is set.
  		if(!is.na(slope.test)){
  		commonslopetestval <- slope.com(mf[,1], mf[,2], mf[,3], alpha=alpha, 
                                      slope.test=slope.test, intercept=intercept, method=method, 
                                      group.names=lv, V=V2, robust=robust)
  		} else {
  		commonslopetestval <- NA
  		}
  		
  		#run group tests
  		if(grouptest == "elevcom"){
  			if(!intercept)stop("Cannot perform elevation test without fitted intercept.")
  			grouptestresult <- elev.com(mf[,1], mf[,2], mf[,3], alpha=alpha, 
                                    method=method, group.names=lv, V=V2, robust=robust)
  		}
  		if(grouptest == "shiftcom"){
  			grouptestresult <- shift.com(mf[,1], mf[,2], mf[,3], intercept=intercept, 
                                     method=method, group.names=lv, V=V2, robust=robust)
  		}
  		if(grouptest == "slopecom")grouptestresult <- ""  #<-- already stored in commonslopetest
  		
		}
    
	 } else {
	
	  # single group
		ngroups<-1
		V2 = array(V, c(dim(V), ngroups)) 
		grps <- as.factor(rep("all", length(mf[,1])))
		lv <- levels(grps)
		commonslopetest <- NA
		commonslopetestval <- NA
		grouptestresult <- ""
    
	}
	 
	#Calculate stuff for each group. Get the sma coefficients.
	coeff <- list(); n<- list(); r2<- list(); pval <- list(); from <- list()
	to<-list(); slopetest <-list(); elevtest <-list()
		
	for(i in 1:ngroups){
		X <- mf[grps == lv[i],2]
		Y <- mf[grps == lv[i],1]
		
		#groupsize
		n[[i]] <- length(X)
	
		#sma coefficients 
		coeff[[i]] <- line.cis(Y,X,intercept=intercept, method=method, 
			alpha=alpha, robust=robust, V=V2[,,i], ...)   

		# correlation and P-value
		if(intercept){
			r2[[i]]<- cor(X, Y)^2
			pval[[i]] <- cor.test(X, Y, method = "pearson")$p.value
		} else {
			r2[[i]] <- sum(X*Y)^2/(sum(X^2) * sum(Y^2))
			pval[[i]] <- 1 - pf(r2[[i]]/(1-r2[[i]])*(n[[i]]-1),1,n[[i]]-1)  
		}
	  
      	# Test slope against some specified value
     	if(!is.na(slope.test)){
			slopetest[[i]] <- slope.test(Y,X,  test.value=slope.test, method=method, 
				alpha=alpha, V=V2[,,i], intercept=intercept, robust=robust)
			slopetestdone <- TRUE
		} else {
			slopetest[[i]] <- slope.test(Y,X,  test.value=NA, method=method, 
				alpha=alpha, V=V2[,,i], intercept=intercept, robust=robust)
			slopetestdone <- FALSE
		}
	
		# Test elevation against some specified value
		if(!is.na(elev.test)){
			if(!intercept)stop("Cannot perform elevation test without fitted intercept.")
				elevtest[[i]] <- elev.test( Y,X, test.value=elev.test, method=method, alpha=alpha, robust=robust, V=V2[,,i])
				elevtestdone <- TRUE
		} else {
				elevtest[[i]] <- elev.test( Y,X, test.value=NA, method=method, alpha=alpha, robust=robust, V=V2[,,i])
				elevtestdone <- FALSE
		}
      	
      	#determine range of fitted values (as X value)
        B <- coeff[[i]][2,1]
		a <- coeff[[i]][1,1]
		
        if(method=="SMA"){
  	    	from[[i]] <- (min(Y+B*X) - a)/(2.0*B) 
  	    	to[[i]] <-(max(Y+B*X)-a)/(2.0*B)
  	    } else if (method =="MA"){
  	    	from[[i]] <- (min(X+B*Y) - B*a)/(1+B^2)
  	    	to[[i]] <-(max(X+B*Y) - B*a)/(1+B^2)
   	    } else if (method == "OLS"){
			from[[i]] <- min(X)
			to[[i]] <- max(X)
		}
		
  	   	if(log %in% c("x","xy")){
			from[[i]] <- 10^from[[i]]
			to[[i]] <- 10^to[[i]]
		}    
	}
	
	# apply group names to new variables, if more than one group.
	if(ngroups > 1){
		names(coeff) <- lv
		names(n) <- lv
		names(r2) <- lv
		names(pval) <- lv
		names(from) <- lv
		names(to) <- lv
	}

	# coefficients under H0 (nullcoef)
	nullcoef <- NA
	if(grouptest == "none" & slopetestdone){   # sm2
		b <- slope.test
		a <- mean(Y) - b*mean(X)
		nullcoef <- c(a,b)
	}
	if(grouptest == "none" & elevtestdone){    # sm3
		a <- elev.test
		b <- NA
		nullcoef <- c(a,b)
	}
	if(grouptest == "slopecom" & !slopetestdone & !elevtestdone){ #sm4
		
		if(method == "OLS")
			nullcoef <- NA
		else {
		bcom <- commonslopetest$b
		nullcoef <- vector("list",ngroups)
		for(i in 1:ngroups){
			X <- mf[grps == lv[i],2]
			Y <- mf[grps == lv[i],1]
			a <- mean(Y) - bcom*mean(X)
			b <- bcom
			nullcoef[[i]] <- matrix(rep(NA,6),nrow=2)
			nullcoef[[i]][,1] <- c(a,b)
		}
		}
	}
	if(grouptest == "slopecom" & slopetestdone & !elevtestdone){ # sm5

		if(method == "OLS")
			nullcoef <- NA
		else {
		nullcoef <- vector("list",ngroups)
		for(i in 1:ngroups){
			X <- mf[grps == lv[i],2]
			Y <- mf[grps == lv[i],1]
			b <- slope.test
			a <- mean(Y) - b*mean(X)
			
			nullcoef[[i]] <- matrix(rep(NA,6),nrow=2)
			nullcoef[[i]][,1] <- c(a,b)
		}
		}
	}
	if(grouptest %in% c("elevcom","shiftcom") & !slopetestdone & !elevtestdone){
		
		# Overwrite the coefficients : these are assuming a common slope.
		bcom <- commonslopetest$b
		
		for(i in 1:ngroups){
			X <- mf[grps == lv[i],2]
			Y <- mf[grps == lv[i],1]
			a <- mean(Y) - bcom*mean(X)
			b <- bcom
			coeff[[i]][,1] <- c(a,b)
			coeff[[i]][2,2:3] <- commonslopetest$ci
			
			# Fix January 2012. New elev.com now reports CIs by group.
			if(grouptest=="elevcom")coeff[[i]][1,2:3] <- grouptestresult$as[i,2:3]
		}
		
		if(grouptest == "elevcom"){
			nullcoef <- vector("list",ngroups)
			for(i in 1:ngroups){
				a <- grouptestresult$a
				b <- bcom
				nullcoef[[i]] <- matrix(rep(NA,6),nrow=2)
				nullcoef[[i]][,1] <- c(a,b)
			}
		} else {
			nullcoef <- coeff
		}
	}
	
	l <- list()
	l$coef <- coeff
	l$nullcoef <- nullcoef
	l$commoncoef <- commonslopetest
	l$commonslopetestval <- commonslopetestval
	l$alpha <- alpha
	l$method <- method
	l$intercept <- intercept
	l$call <- match.call()
	l$data <- mf
	l$log <- log
	l$variables <- names(mf)
	l$origvariables <- all.vars(match.call()$formula)
  l$formula <- formula
	l$groups <- lv
	l$groupvarname <- if(ncol(mf) == 3)names(mf)[3] else NA
	l$gt <- grouptest
	l$gtr <- grouptestresult
	l$slopetest <- slopetest
	l$elevtest <- elevtest
	l$slopetestdone <- slopetestdone
	l$elevtestdone <- elevtestdone
	l$n <- n
	l$r2 <- r2
	l$pval <- pval
	l$from <- from
	l$to <- to
	sm <- list()
	f <- function(x)unname(unlist(x))
	
	for(i in 1:ngroups){
		sm[[i]] <- list(group=l$groups[i], n=f(l$n[i]), r2=f(l$r2[i]), pval=f(l$pval[i]),
		              Slope=l$coef[[i]][2,1], Slope_lowCI = l$coef[[i]][2,2], Slope_highCI = l$coef[[i]][2,3],  
					        Int = l$coef[[i]][1,1], Int_lowCI = l$coef[[i]][1,2], Int_highCI = l$coef[[i]][1,3],
					        Slope_test = if(all(is.na(slopetest)))NA else f(l$slopetest[[i]]$test.value), 
					        Slope_test_p= if(all(is.na(slopetest)))NA else f(l$slopetest[[i]]$p), 
					        Elev_test = if(all(is.na(elevtest)))NA else f(l$elevtest[[i]]$test.value),
					        Elev_test_p= if(all(is.na(elevtest)))NA else f(l$elevtest[[i]]$p))
	}
	tmp <- as.data.frame(do.call("rbind",sm))
	l$groupsummary <- as.data.frame(lapply(tmp, unlist))
	
	class(l) <- "sma"
	
	
	if(multcomp & ngroups == 2)
		warning("Multiple comparisons not performed, because there are only two groups (there is only one comparison to do)!")
	
	if(multcomp & ngroups > 2){
	
		# Pairwide group combinations:
		pairmat <- combn(l$groups, 2)
		npairs <- ncol(pairmat)
		paircall <- list()
		
		# Call sma with a data subsets:
		for(k in 1:npairs){
			
			# List of arguments as 'sma' was called.
			thiscall <- as.list(match.call(expand.dots = TRUE))[-1]
			
			# Set multcomp to FALSE
			thiscall$multcomp <- FALSE
			
			# Make data subset.
			#datasubs <- mf[mf[,3] %in% pairmat[,k],]
			datasubs <- data[data[,l$groupvarname] %in% pairmat[,k],]
			datasubs <- droplevels(datasubs)  # drop empty levels.
			thiscall$data <- datasubs
			
			# Set dummy argument, to keep track of what type of call this is:
			thiscall$dummy <- TRUE
			
			# Re-Call sma, using the data subset.
			paircall[[k]] <- do.call(what="sma", args=thiscall)
		}
		
		# Slope p values.
		multres <- as.data.frame(cbind(t(pairmat)))
		
		# Slope test (default, stored in commoncoef element).
		if(l$gt == "" | l$gt == "slopecom"){
			pvalsSlope <- sapply(paircall, function(x)x$commoncoef$p)
			multres$Pval <- pvalsSlope
			multcompdone <- "slope"
			
			# Test stat., df. 
			multres$TestStat <- sapply(paircall, function(x)x$commoncoef$LR)
			multres$df <- sapply(paircall, function(x)x$commoncoef$df)
			
			# Slope values
			multres$Slope1 <- sapply(paircall, function(x)x$commoncoef$bs[1,1])
			multres$Slope2 <- sapply(paircall, function(x)x$commoncoef$bs[1,2])
			
		}
		# elevation test, if performed.
		if(l$gt == "elevcom"){
			pvalsElev <- sapply(paircall, function(x)x$gtr$p)
			multres$Pval <- pvalsElev
			multcompdone <- "elevation"
			
			# Test stat., df. 
			multres$TestStat <- sapply(paircall, function(x)x$gtr$stat)
			multres$df <- sapply(paircall, function(x)x$gtr$df)
			
			# Elevation values.
			multres$Elev1 <- sapply(paircall, function(x)x$gtr$as[1])
			multres$Elev2 <- sapply(paircall, function(x)x$gtr$as[2])
			
		}
		if(l$gt == "shiftcom"){
			pvalsShift <- sapply(paircall, function(x)x$gtr$p)
			multres$Pval <- pvalsShift
			multcompdone <- "shift"
			
			# Test stat., df. 
			multres$TestStat <- sapply(paircall, function(x)x$gtr$stat)
			multres$df <- sapply(paircall, function(x)x$gtr$df)
			
			# Shift values.
			multres$Shift1 <- sapply(paircall, function(x)x$gtr$f.mean[1])
			multres$Shift2 <- sapply(paircall, function(x)x$gtr$f.mean[2])
		}
		
		# Label first two variable names in multcomp result.
		names(multres)[1:2] <- paste(l$groupvarname, "_", 1:2, sep="")
		
		# P-value Bonferroni adjustment
		if(multcompmethod == "adjusted"){
			adjusted.p.multi <- 1-(1-multres$Pval)^sum(multres$Pval >= 0,na.rm=TRUE)
			multres$Pval <- adjusted.p.multi
		}
		l$multcompresult <- multres
		l$multcompdone <- multcompdone
		l$multcompmethod <- multcompmethod

	}
	if(!multcomp){
		l$multcompresult <- NA
		l$multcompdone <- "none"
		l$multcompmethod <- NA
	}
	
	# Now print warning that X or Y values were deleted (complicated here, because only print once).
	thiscall <- as.list(match.call(expand.dots = TRUE))[-1]
	if(!("dummy" %in% names(thiscall)) && xdel)warning("Deleted negative and/or zero values in X variable.")
	if(!("dummy" %in% names(thiscall)) && ydel)warning("Deleted negative and/or zero values in Y variable.")
	
    return(l)
}
