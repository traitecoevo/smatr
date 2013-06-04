fitted.sma <- function(object, type = "fitted", X=NULL,Y=NULL, centered=TRUE, ...){


	obj <- object
	
  # Use 'new data', to mimic a predict.sma function.
  if(!is.null(X) || !is.null(Y)){
    
    if(obj$gt != "none")
      stop("Cannot provide fitted values for new X and Y if groups were present in original model fit.")
    
    if(length(X) != length(Y))
      stop("When providing new X and Y, they must be the same length.")
    
  } else {
    
    X <- obj$data[,2]
    Y <- obj$data[,1]
    
  }
  
	if(obj$gt == "none"){ #single line			
		# coefficients
		p <- coef(obj)
		a <- p[1]
		B <- p[2]
	} else {  			  #lines by group
		# grouping
		gr <- obj$data[,3]	
		
		# coefficients	
		p <- coef(obj)
    
		# get group slopes
		preddfr <- data.frame(X,gr)
		p$gr <- rownames(p)
		preddfr <- merge(preddfr,p,by="gr",sort=FALSE)
		a <- preddfr$elevation
		B <- preddfr$slope
	}
	
	if(type=="residuals"){
		OUT <- Y - (a + B*X)   #centre around zero
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



