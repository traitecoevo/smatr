#' Returns fitted values
#' 
#' @description Returns "fitted values" of a (standardized) major axis fit.
#' 
#' @details "Fitted values" are calculated using \code{y+bx} for SMA or \code{by+x} for
#' MA (see Warton et al. 2006, especially Table 4). Note these values are
#' calculated differently to ordinary linear regression, and they cannot be
#' interpreted as predicted values. The "fitted values" should be interpreted
#' as measuring how far along the fitted axis each point lies, and are used in
#' checking assumptions (via residual vs. fitted value plots).
#' 
#' Fitted values may be computed for new data, using the \code{newdata}
#' arguments. Be aware that the names of the variables need to be the same as
#' in the data used in the original model fit. Also, if the original fit used
#' \code{log='xy'}, for example, this transformation will also be applied to
#' your \code{newdata} (so don't do the transformation yourself).
#' 
#' @param object Object of class \code{sma}.
#' @param type Either 'residuals', or 'fitted'.
#' @param newdata New data for which to provide fitted values.
#' @param centered Logical. If TRUE (the default) returns the zero-centered
#' fitted values.
#' @param \dots Further arguments are currently ignored.
#' @seealso \code{\link{sma}},\code{\link{residuals.sma}}
#' @references Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006)
#' A review of bivariate line-fitting methods for allometry.  \emph{Biological
#' Reviews} \bold{81}, 259-291.
#' @keywords misc
#' @export
fitted.sma <- function(object, type = "fitted", newdata=NULL, centered=TRUE, ...){


	obj <- object
	
  # Use 'new data', to (sort of) mimic a predict.sma function.
	if(!is.null(newdata))	{
	  newdat <- model.frame(obj$formula,data=newdata)
	  X <- newdat[,2]
	  Y <- newdat[,1]
    
    # Log transformations
    if(grepl("x",obj$log))X <- log10(X)
	  if(grepl("y",obj$log))Y <- log10(Y)
    
    if(ncol(newdat)>2){
 	    gr <- newdat[,3]
      
      # Check if levels are known
      if(any(!levels(gr) %in% levels(obj$data[,3])))
        stop("Your newdata contains some levels with different names to those used in the original model fit.\n This makes me unhappy.")
       
    }
    
	} else {
    
    X <- obj$data[,2]
    Y <- obj$data[,1]
    
    if(obj$gt != "none")
      gr <- obj$data[,3]	
	}
  
	if(obj$gt == "none"){ #single line			
		# coefficients
		p <- coef(obj)
		a <- p[1]
		B <- p[2]
	} else {  			  #lines by group
	
		# coefficients	
		p <- coef(obj)
    
		# get group slopes
		p$gr <- rownames(p)
		preddfr <- data.frame(X = X, Y = Y, gr = gr, order = 1:length(X))      
		preddfr <- merge(preddfr, p, by = "gr")
		preddfr <- preddfr[sort.list(preddfr$order), ]  
		a <- preddfr$elevation
		B <- preddfr$slope
    X <- preddfr$X
    Y <- preddfr$Y
	}
	
	if(type=="residuals"){
		OUT <- Y - (a + B*X)
	}
	else if(type=="fitted"){
		if(obj$method=="SMA"){
      OUT <- Y + B*X		
		} else if(obj$method=="MA"){
			OUT <- B*Y + X		
	} else {
			OUT <- X		
	}
		if(centered)OUT <-OUT - mean(OUT)  #centre around zero
	}
return(OUT)
}


# data(leaflife)
# leaf.low.soilp <- subset(leaflife, soilp == 'low')
# 
# com.test <- sma(longev~lma*rain, log="xy", data=leaf.low.soilp)
# elev.res <- sma(longev~lma+rain, log="xy", data=leaf.low.soilp)
# shift.res <- sma(longev~lma+rain, type="shift", log="xy", data=leaf.low.soilp)
# 
# # newdat <- leaf.low.soilp
# i <- 1:10
# fitted(com.test, centered=FALSE)
# fitted(com.test, newdata=leaf.low.soilp[i,], centered=FALSE)
# 
# nd <- leaf.low.soilp[i,]
# levels(nd$rain) <- c("hello","there")
# fitted(com.test, newdata=nd, centered=FALSE)



