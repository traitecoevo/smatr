#' Common slope test amongst several allometric lines
#' 
#' @description Test if several major axis or standardised major axis lines share a common
#' slope. This can now be done via \code{sma(y~x*groups)}, see help on the
#' \code{\link{sma}} function.
#' 
#' @details For several bivariate groups of observations, this function tests if the
#' line-of-best-fit has a common slope for all samples, when the
#' line-of-best-fit is estimated using the major axis, standardised major axis,
#' or a more general version of these methods in which the error variance ratio
#' is estimated from the data.
#' 
#' The test assumes the following: \enumerate{ \item each group of observations
#' was independently sampled \item y and x are linearly related within each
#' group \item residuals independently follow a normal distribution with equal
#' variance at all points along the line, within each group } Note that we do
#' not need to assume equal variance across groups, unlike in the standard test
#' for common slope for linear regression.
#' 
#' The assumptions can be visually checked by plotting residual scores against
#' fitted axis scores, and by constructing a Q-Q plot of residuals against a
#' normal distribution, available using the \code{\link{plot.sma}} function.
#' 
#' Setting \code{robust=TRUE} fits lines using Huber's M estimation, and
#' modifies the test statistic as proposed in Taskinen & Warton (in review).
#' 
#' The common slope is estimated from a maximum of 100 iterations, convergence
#' is reached when the change in b is \eqn{< 10^{-6}}{< 10^-6}.
#' 
#' @param y The Y-variable for all observations (as a vector)
#' @param x The X-variable for all observations (as a vector)
#' @param groups Coding variable identifying which group each observation
#' belongs to (as a factor or vector)
#' @param method The line fitting method: \describe{ \item{'SMA' or
#' 1}{standardised major axis (this is the default)} \item{'MA' or 2}{major
#' axis} \item{'lamest' or 3}{Error variance ratio is estimated from the data}
#' }
#' @param alpha The desired confidence level for the 100(1-alpha)\% confidence
#' interval for the common slope. (Default value is 0.05, which returns a 95\%
#' confidence interval.)
#' @param data Deprecated. Use with() instead (see Examples).
#' @param intercept (logical) Whether or not the line includes an intercept.
#' \describe{ \item{FALSE}{ no intercept, so the line is forced through the
#' origin } \item{TRUE}{ an intercept is fitted (this is the default) } }
#' @param robust If TRUE, uses a robust method to fit the lines and construct
#' the test statistic.
#' @param V The estimated variance matrices of measurement error, for each
#' group. This is a 3-dimensional array with measurement error in Y in the
#' first row and column, error in X in the second row and column, and groups
#' running along the third dimension. Default is that there is no measurement
#' error.
#' @param group.names (optional: rarely required). A vector containing the
#' labels for `groups'. (Only actually useful for reducing computation time in
#' simulation work).
#' @param ci (logical) Whether or not to return a confidence interval for the
#' common slope.
#' @param bs (logical) Whether or not to return the slopes for the separate
#' groups, with confidence intervals.
#' @param slope.test If a value provided, tests the common slope fit against
#' this value.
#' @return \item{lr }{The (Bartlett-corrected) likelihood ratio statistic
#' testing for common slope} \item{p }{The P-value of the test. This is
#' calculated assuming that lr has a chi-square distribution with (g-1) df, if
#' there are g groups} \item{b }{The common slope estimate} \item{varb }{The
#' sample variance of the common slope} \item{ci }{A 100(1-alpha)\% confidence
#' interval for the common slope} \item{lambda }{The error variance ratio - the
#' ratio of error variance in y to error variance in x. For MA, this is assumed
#' to be 1. for SMA, this is assumed to be \eqn{b^2}{b^2}. For the `lamest'
#' method, the error variance ratio is estimated from the data under the common
#' slope assumption.} \item{bs }{The slopes and confidence intervals for data
#' from each group.}
#' @author Warton, D.I.\email{David.Warton@@unsw.edu.au}, J. Ormerod, & S.
#' Taskinen
#' @seealso \code{\link{sma}}, \code{\link{line.cis}}, \code{\link{elev.com}},
#' \code{\link{shift.com}}
#' @references Warton D. I. and Weber N. C. (2002) Common slope tests for
#' bivariate structural relationships.  \emph{Biometrical Journal} \bold{44},
#' 161--174.
#' 
#' Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of
#' bivariate line-fitting methods for allometry.  \emph{Biological Reviews}
#' \bold{81}, 259--291.
#' 
#' Taskinen, S. and D.I. Warton. in review. Robust tests for one or more
#' allometric lines.
#' @keywords htest
#' @export
#' @examples
#' 
#' #load leaf longevity data
#' data(leaflife)
#' 
#' #plot the data, with different symbols for different groups.
#' plot(leaflife$lma, leaflife$longev, type='n', log='xy', xlab=
#'    'leaf mass per area [log scale]', ylab='leaf longevity [log scale]')
#' colours <- c('blue', 'red', 'green', 'yellow')
#' points(leaflife$lma, leaflife$longev,
#'    col=colours[as.numeric(leaflife$site)])
#' legend(55, 5, as.character(unique(leaflife$site)), col=colours,
#'    pch=rep(1,4))
#' 
#' #test for common SMA slope of log(leaf longevity) vs log(LMA),
#' #across species sampled at different sites:
#' fit <- with(leaflife, slope.com(log10(longev), log10(lma), site))
#' fit
#' 
#' #Residual vs fits plots for SMA fit of each site
#' y <- log10(leaflife$longev)
#' x <- log10(leaflife$lma)
#' site <- leaflife$site
#' par( mfrow=c(2,2) )
#' plot(y[site==1] + fit$bs[1,1] * x[site==1], y[site==1] - fit$bs[1,1] 
#'    * x[site==1], xlab='fits (site 1)', ylab='residuals (site 1)')
#' plot(y[site==2] + fit$bs[1,2] * x[site==2], y[site==2] - fit$bs[1,2]
#'    * x[site==2], xlab='fits (site 2)', ylab='residuals (site 2)')
#' plot(y[site==3] + fit$bs[1,3] * x[site==3], y[site==3] - fit$bs[1,3]
#'    * x[site==3], xlab='fits (site 3)', ylab='residuals (site 3)')
#' plot(y[site==4] + fit$bs[1,4] * x[site==4], y[site==4] - fit$bs[1,4]
#'    * x[site==4], xlab='fits (site 4)', ylab='residuals (site 4)')
#' 
#' #Test for common SMA slope amongst species at low rainfall sites
#' #with different levels of soil nutrients
#' leaf.low.rain <- leaflife[leaflife$rain=='low',]
#' with(leaf.low.rain, slope.com(log10(longev), log10(lma), soilp))
#' 
#' #test for common MA slope:
#' with(leaflife, slope.com(log10(longev), log10(lma), site, method='MA'))
#' 
#' #test for common MA slope, and produce a 90% CI for the common slope:
#' with(leaflife, slope.com(log10(longev), log10(lma), site,  method='MA', alpha=0.1))
#' 
#' 
slope.com <- function( y, x, groups, method="SMA", alpha=0.05, data=NULL, 
                       intercept=TRUE, robust=FALSE, V=array( 0, c( 2,2,length(unique(groups)) ) ), 
                       group.names=sort(unique(groups)), ci=TRUE, bs=TRUE, slope.test=NULL ){
  
    if ( nargs() < 3 ){
        stop('Sorry, no can do without three arguments -- Y, X, GROUPS')
    }

    if(!is.null(data))
      stop("'data' argument no longer supported.")
    
    dat    <- cbind(y, x)
    g      <- length(group.names)

    # Find sample size, variances for each group:
    n      <- matrix( 0, g, 1 )
    r.factor      <- matrix( 0, g, 1 )
    res.df <- matrix( 0, g, 1 )
    z      <- matrix( 0, g, 3 )
    do.bs  <- bs
    bs     <- matrix( NA, 3, g, dimnames=list(c("slope","lower.CI.lim","upper.CI.lim"),group.names) )
    for (i in 1:g)
    {
        iref   <- ( groups==group.names[i] )
        iref   <- iref & ( is.na(x+y) == FALSE )
        n[i]   <- sum(iref)
        if ( robust ){
	       if (!intercept){
	    	  stop("Sorry, robust estimation without an intercept term not yet implemented.")
	       }
         else
	       {
            if (n[i]>1)
            { 
                      q  <- pchisq(3,2)
                      S  <- huber.M(dat[iref,])
		                  xi <- S$cov-V[, , i]  
        	            means <- S$loc
                     
		            # get robust.factor for group i (multiplier on variance matrix):
	              r.factor[i] <- robust.factor(dat[iref,],q)[1] 
		        }
	          else if (n[i]==1)
	                  { 
                    xi <- matrix(0,2,2)
                    r.factor[i] <- 0  
                    } #leave as zero for n[i]=1
          }
          z[i,]     <- c( xi[1,1], xi[2,2], xi[1,2] )

           if (do.bs & n[i]>1){
                slopei    <- slope.test(y[iref], x[iref], method=method, alpha=alpha, 
                                        V=V[,,i], intercept=intercept, robust=TRUE)
                bs[,i]    <- c(slopei$b, slopei$ci)
           }
        }	else {
           r.factor[i]<-1
       
           if (!intercept) {
	  	        xi <- t(dat[iref, ]) %*% dat[iref, ] / n[i] - V[, , i]
	         } else  {
                if (n[i]>1){ 
                  xi <- cov(dat[iref, ]) - V[, , i] 
                } else if (n[i]==1){ 
                  xi <- matrix(0,2,2)  #leave as zero for n[i]=1
                }
           }

           z[i,] <- c( xi[1,1], xi[2,2], xi[1,2] )
           
           if (do.bs & n[i]>1) {
                slopei <- slope.test(y[iref], x[iref], method=method, 
                                        alpha=alpha, V=V[,,i], intercept=intercept,robust=FALSE )
                
                bs[,i] <- c(slopei$b, slopei$ci)
           }
        }
    }
    if(any(z[,1:2]<=0))
      stop("A measurement error variance is too large (it is larger than the sample variance in one of the observed variables!)")
    
    if (!intercept){ 
      res.df <- n-1 
    } else { 
      res.df <- n-2 
    }

    # Find common slope:
    lambda <- 1 #only actually used for the major axis.
    if (is.null(slope.test)) {
        res    <- b.com.est( z, n, method, lambda, res.df=res.df )
    }
    #input slope.test as common slope value.
    else { 
        if ( method == 1 | method == 'SMA') { lambda <- slope.test^2 }
        res <- list(b=slope.test, bi=slope.test, l1=NA, l2=NA, lambda=lambda )
    }
    
    # Calculate LR:
    dets <- z[,1]*z[,2] - z[,3]^2 #This is l1*l2 under Halt.
    arguments <- list( l1=dets, l2=1, z=z, n=n, method=method, crit=0, 
                       lambda=lambda, res.df=res.df, r.factor=r.factor)
    
    LR     <- lr.b.com(res$b, arguments) 
    
    
    # if lambda is being estimated, check endpoint LR values:
    if (method == 3 | method == 'lamest')  {
        res0      <- b.com.est( z, n, 2, lambda=10^-9, res.df ) # to find est when lambda=0
        arguments <- list( l1=dets, l2=1, z=z, n=n, method=method, crit=0, 
                           lambda=10^-9, res.df=res.df, r.factor=r.factor)
        LR0       <- lr.b.com(res0$b, arguments) 
        resInf    <- b.com.est( z, n, 2, 10^9, res.df ) # to find est when lambda=inf
        arguments <- list( l1=dets, l2=1, z=z, n=n, method=method, crit=0, 
                           lambda=10^9, res.df=res.df, r.factor=r.factor)
        LRinf     <- lr.b.com(resInf$b, arguments) 
        LR        <- min(LR,LR0,LRinf)
        if ( LR==LR0 )res <- res0 
        if ( LR==LRinf )res <- resInf
    }
    
    # Record values for arguments separately
    b      <- res$b
    bi     <- res$bi
    l1     <- res$l1
    l2     <- res$l2
    lambda <- res$lambda

    # Calculate P-value:
    if (is.null(slope.test)){
        df <- g - 1 - sum(n<=1)  #don't count any singleton or empty groups in df
    } else {
        df <- g - sum(n<=1)  #if common slope is given, don't subtract a df for its estimation
    }
    Pvalue <- 1 - pchisq( LR, df )

    # Calculate a CI for common slope, if estimated
    if (is.null(slope.test)){
        if (method == 1 | method == 'SMA'){
          
           # Getting variance of common slope
           varBs <- ( l2/l1 + l1/l2 + 2)^(-1) *  ( 4*b^2 ) * r.factor
           l12   <- ( l1 + l2 )^2 / ( l1*l2 )
      
        } 
        else
        if (method == 2 | method == 'MA'){
           varBs <- ( l2/l1 + l1/l2 - 2)^(-1) * ( lambda + b^2 )^2 * r.factor
           l12 <- ( l1 - l2 )^2 / ( l1*l2 )
        }
        
        #Still work to be done to calculate CI for lamest.
        if (method == 3 | method == 'lamest'){
           varBs <- NA
        }

        varB <- sum( l12^2 * varBs * res.df )/(sum( l12 * res.df ))^2
    }
    else{
        varB <- NA
        ci   <- FALSE
    }

    crit <- qchisq( 1 - alpha, 1 )
    if (method == 3 | method == 'lamest') {
       ci <- FALSE
    }
    
    bCI=NA
    if (ci){
       bCI  <- com.ci( b, varB, crit, z, n, l1, l2, method, lambda, res.df, r.factor)
    }
    
    if (lambda==10^-9) lambda <- 0 
    if (lambda==10^9) lambda  <- Inf 
    
    return(list( LR=LR, p=Pvalue, b=b, ci=bCI, varb=varB, lambda=lambda, bs=bs, df=df ))
}

