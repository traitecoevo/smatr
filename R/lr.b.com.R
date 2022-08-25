#' Likelihood ratio statistic, assuming a common slope (called by slope.com)
#' 
#' 
#' @param b Common slope estimate.
#' @param arguments A list of other required arguments.
#' @author Warton, D. \email{David.Warton@@unsw.edu.au}, translated to R by
#' Ormerod, J. 2005-12-08
#' @seealso \code{\link{slope.com}}
#' @keywords internal
lr.b.com <- function( b, arguments )
{
    
    l1       <- arguments$l1
    l2       <- arguments$l2
    z        <- arguments$z
    n        <- arguments$n
    method   <- arguments$method
    crit     <- arguments$crit 
    lambda   <- arguments$lambda
    res.df   <- arguments$res.df 
    r.factor <- arguments$r.factor

    if (method == 1 | method == 'SMA'){
        
        if (b == 0) {
            b <- 10^-6
        }
        l1b <- ( b^2*z[,2] + 2*b*z[,3] + z[,1] )/2/abs(b)
        l2b <- ( b^2*z[,2] - 2*b*z[,3] + z[,1] )/2/abs(b)
    }
    else if (method == 2 | method == "MA" | method == 3 | method == "lamest" ){
        l1b <- ( lambda^2*z[,2] + 2*lambda*b*z[,3] + b^2*z[,1] )/( lambda + b^2 )
        l2b <- ( b^2*z[,2] - 2*b*z[,3] + z[,1] )/( lambda + b^2 )
    }

     
    lr <- sum( ( res.df - 0.5 ) * log (1 + ( l1b*l2b/l1/l2 - 1 ) / r.factor ), na.rm=TRUE ) - crit
}

