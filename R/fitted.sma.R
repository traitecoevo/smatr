fitted.sma <- function(object, ..., type = "fitted"){

	obj <- object
	
		X <- obj$data[,2]
		Y <- obj$data[,1]

	if(obj$gt == "none"){ #single line			
		#coefficients
		p <- coef(obj)
		a = p[1]
		B = p[2]
	} else {  			  #lines by group
		#grouping
		gr <- obj$data[,3]	
		
		#coefficients	
		p <- coef(obj)
		#get group slopes
		preddfr <- data.frame(X,gr)
		p$gr <- rownames(p)
		preddfr <- merge(preddfr,p,by="gr",sort=FALSE)
		a = preddfr$elevation; 
		B = preddfr$slope
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
		OUT <-OUT - mean(OUT)  #centre around zero
	}
return(OUT)
}