#' Calculates a confidence interval for a common slope (called by slope.com)
#' 
#' @param b Common slope estimate.
#' @param varb Estimated variance of the common slope.
#' @param crit Critical value to be used in confidence interval calculation.
#' @param z Variances and covariances of each group.
#' @param n Sample sizes of each group.
#' @param l1 Variance of fitted axis scores, for each group.
#' @param l2 Variance of fitted residuals, for each group.
#' @param method See slope.com for details.
#' @param lambda Error variance ration (implied by choice of method.
#' @param res.df Residual degrees of freedom, for each group.
#' @param r.factor A vector of "robust factors" for each group, default value
#' 1. Variance estimates are rescaled by this factor when using robust
#' inference.
#' @author Warton, D.I.\email{David.Warton@@unsw.edu.au}, J. Ormerod, & S.
#' Taskinen
#' @seealso \code{\link{slope.com}}
#' @keywords internal
com.ci <- function( b, varb, crit, z, n, l1, l2, method, lambda, res.df, r.factor)
{
    b.ci <- c(NA,NA)
    arguments <- list( l1=l1, l2=l2, z=z, n=n, method=method, crit=crit, lambda=lambda, res.df=res.df, r.factor=r.factor )
    #check if limits have opposite sign
    val.b <- lr.b.com(b,arguments)
    b.m   <- b - 2*sqrt(crit*varb)
    b.p   <- b + 2*sqrt(crit*varb)
    val.m <- lr.b.com(b.m,arguments)
    val.p <- lr.b.com(b.p,arguments)
    #if necessary, move limits further from b (small sample issues)
    if ( val.m*val.b > 0 )
        {
         b.m <- b - 4*sqrt(crit*varb)
         val.m <- lr.b.com(b.m,arguments)
        }
    if ( val.p*val.b > 0 )
        {
         b.p <- b + 4*sqrt(crit*varb)
         val.p <- lr.b.com(b.p,arguments)
        }
    #if still problems, move further and adjust for possibly 0 variance
    if ( val.m*val.b > 0 )
        {
         b.m <- b - 8*sqrt(crit*(varb+0.1))
         val.m <- lr.b.com(b.m,arguments)
        }
    if ( val.p*val.b > 0 )
        {
         b.p <- b + 8*sqrt(crit*(varb+0.1))
         val.p <- lr.b.com(b.p,arguments)
        }
    res <- uniroot( lr.b.com, c(b.m, b ), tol = 0.0001, arguments=arguments )
    b.ci[1] <- res$root
    res <- uniroot( lr.b.com, c(b , b.p ), tol = 0.0001, arguments=arguments )
    b.ci[2] <- res$root

    if ( b.ci[1]==b.ci[2] )
    {
        str("Same limits - unable to find a different solution!?")
    }
    b.ci
}
