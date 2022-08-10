#' Measurement error variance estimation from repeated measures
#' 
#' @description Estimates the average variance matrix of measurement error for a set of
#' subjects with repeated measures.
#' 
#' @details This function allows the estimation of measurement error variance, given a
#' set of repeated measures on different subjects. Measurement error variance
#' is estimated separately for each subject, then averaged across subjects.
#' This provides terms that can be used to correct for measurement error in
#' allometric analyses.
#' 
#' Any number of variables can be specified in the data matrix for measurement
#' error calculation. If more than one variable is specified, the covariance of
#' measurement error is estimated from the repeated measures as well as the
#' variance.
#' 
#' As well as the estimated measurement error variance, a data matrix is
#' returned which contains the averages of the repeated measures for each
#' subject.
#' 
#' @param datameas A data matrix containing the repeated measures of
#' observations, with each variable in a different column and all observations
#' on all subjects in different rows of the same column.
#' @param id An id vector identifying the subject being measured for each
#' observation in the data matrix.
#' @param data Deprecated. Use with() instead (see Examples).
#' @return \item{V}{A matrix containing the average variances and average
#' convariances of the repeated measures of subjects.}
#' 
#' \item{dat.mean}{A matrix containing the values for each subject, averages
#' across repeated measures. Subjects are in rows, variables in columns.}
#' @author Warton, D. \email{David.Warton@@unsw.edu.au}, translated to R by
#' Ormerod, J. 2005-12-08
#' @seealso \code{\link{line.cis}}, \code{\link{slope.test}}
#' @references Warton, D.I., I.J. Wright, D.S. Falster and M. Westoby. 2006.
#' Bivariate line-fitting methods for allometry. \emph{Biological Reviews}
#' \bold{81}, 259--291.
#' @keywords models regression
#' @export
#' @examples
#' 
#' #load the individual level leaf example dataset
#' data(leafmeas)
#' 
#' #Estimate measurement error variance matrix, store in "meas.vr"
#' meas.vr <- meas.est(leafmeas[,3:4], leafmeas$spp)
#' 
meas.est <- function( datameas, id, data=NULL )
{
    if ( nargs() != 2 )
    {
        stop("An id vector is required, to identify which subject each measurement belongs to.")
    }
#     if ( is.null(data)==FALSE )
#     {
#         attach(data)
#     }
    
    
    if(!is.null(data))
      stop("'data' argument no longer supported.")
    
    
    datameas <- as.matrix( datameas )
    siz <- dim( datameas )
    if ( length(id)!=siz[1] )
    {
        stop("The id vector must have the same number of rows as the data matrix")
    }

    idlabels <- sort( unique( id ) )
    n        <- length( idlabels )
    ni       <- matrix( 0, n, siz[2] )
    dat      <- matrix( NA, n, siz[2] )
    vrs      <- rep( NA, siz[2] * siz[2] * n )
    dim(vrs) <- c( siz[2], siz[2], n )

    is.OK    <- is.finite( apply( datameas , 1, sum ) ) #the rows with no nan's

    for ( i in 1:n )
    {
        ref      <- id==idlabels[i] & is.OK
        ni[i]    <- sum( as.numeric(ref) )
        dat[i,]  <- apply( as.matrix( datameas[ ref, ] ), 2, mean )
        if ( ni[i] > 1 )
            { vrs[, , i] <- var( datameas[ ref, ] ) / ni[i] }
    }

    V <- apply(vrs, 1:2, mean, na.rm=TRUE)

#     if ( is.null(data)==FALSE )
#     {
#        detach(data)
#     }

    list( V=V, dat.mean=dat )
}
