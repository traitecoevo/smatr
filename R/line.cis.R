
line.cis <- function( y, x, alpha=0.05, data=NULL, method="SMA", intercept=TRUE, robust=FALSE, V=matrix(0,2,2), f.crit=0,...)
{

    # instead of attaching
    if (!is.null(data))attach(data) 
	
    dat  <- data.frame( y, x )
    datm <- as.matrix(na.omit(dat))
    n <- nrow(datm)
    
    # was:
    # # Remove NA cases
    # iref <- !is.na(x+y) 
    # n    <- sum(iref)
    #datm <- as.matrix( dat[iref,] )
    # Removed 'iref' everywhere below, since datm already has no missing values.
    
    # if the line is forced through the origin, df are n-1 not n-2
    if ( intercept )
        res.df <- n-2
    else
        res.df <- n-1 

    if (f.crit == 0)f.crit <- qf( 1 - alpha, 1, res.df )

    #if the line is forced through the origin, SS are estimated without centring the data.
    if ( robust )
    {
	    if( intercept )
	    {
		# get robust mean/var matrix:
		q     <- pchisq(3,2)
		S     <- huber.M(datm)
		means <- S$loc
		vr    <- ( S$cov - V) *(n-1)

		# get robust.factor (multiplier on variance matrix):
	      datac <- sweep(datm,2,means,"-")

		# get matrix square root of S$cov:
		apu   <- eigen(S$cov)
		L     <- apu$values
		P     <- apu$vectors
		z     <- datac %*% P%*%(diag(L^(-1/2)))%*%t(P)
	      r     <- sqrt(diag(z%*%t(z)))
	      robust.factor <- mean(alpha.fun(r,2,q)^2)/8
#		print(robust.factor)
	    }
	    else
	    {
		stop("Sorry, robust estimation without an intercept term not yet implemented.")
	    }
    }
    else
    {
      robust.factor <- 1
	if ( intercept )
	{
    	    vr <- ( var(datm) - V )*(n-1) 
 	    means    <- apply(datm,2,mean)
	}
    	else 
    	    vr <- t(datm) %*% datm - V*n
    }	

	
    r   <- vr[1,2] / sqrt( vr[1,1]*vr[2,2] )
    cis <- matrix( 0, 2, 2)

    
    if ( method == 0 | method=="OLS" )
    {
        lab      <- "coef(reg)"
        b        <- vr[1,2] / vr[2,2]
        var.res  <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.b    <- var.res / vr[2,2] * robust.factor
        cis[2,1] <- b - sqrt(var.b)*sqrt(f.crit)
        cis[2,2] <- b + sqrt(var.b)*sqrt(f.crit)
    }
    if ( method==1 | method=="SMA" )
    {
        lab      <- "coef(SMA)"
        b        <- sign( vr[1,2] ) * sqrt( vr[1,1] / vr[2,2] )
        bigb     <- f.crit * ( 1 - r^2 ) / res.df * robust.factor
        cis[2,1] <- b*( sqrt(bigb+1) - sqrt(bigb) )
        cis[2,2] <- b*( sqrt(bigb+1) + sqrt(bigb) )
	  if(b<0) #to ensure the lower limit is the more negative limit
		cis[2,] = cis[2,c(2,1)]
        var.res  <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.b    <- ( vr[1,1] - vr[1,2]^2/vr[2,2] ) / res.df/vr[2,2] * robust.factor
    }
    if ( method==2 | method=="MA" )
    {
	lab      <- "coef(MA)"
        fac      <- vr[1,1] - vr[2,2]
        b        <- ( fac + sqrt( fac^2 + 4*vr[1,2]^2 ) ) / 2 / vr[1,2]
        Q        <- f.crit*( vr[1,1]*vr[2,2] - vr[1,2]^2 ) / res.df * robust.factor
        if ( (fac^2 + 4*vr[1,2]^2 - 4*Q ) < 0 )
        {
            cis[2,1] <- -Inf
            cis[2,2] <-  Inf
        }
	  else
	  {
            cis[2,1] <- (fac + sqrt( fac^2 + 4*vr[1,2]^2 - 4*Q ) ) / 2 / ( vr[1,2] + sqrt(Q) )
            cis[2,2] <- (fac + sqrt( fac^2 + 4*vr[1,2]^2 - 4*Q ) ) / 2 / ( vr[1,2] - sqrt(Q) )
	  	if ( Q>vr[1,2]^2 & fac>0 ) #MA limits overlap Y-axis
	  	{
			warning(paste("Note this CI includes the Y-axis - the actual CI is (",cis[2,1],",infinity) and (-infinity,", cis[2,2], ")"))
		}
	  }
        var.res  <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.fit  <- ( b^2*vr[1,1] + 2*b*vr[1,2] + vr[2,2] ) / res.df
        var.b    <- 1 / ( var.res/var.fit + var.fit/var.res - 2 )*( 1 + b^2 )^2 / res.df * robust.factor
    }

    if (intercept)
    {
        a        <- means[1] - b*means[2]
	  var.a    <- var.res/n + var.b*means[2]^2
        cis[1,1] <- a - sqrt(var.a)*sqrt(f.crit)
        cis[1,2] <- a + sqrt(var.a)*sqrt(f.crit)
    }
    else
    {
        a        <- 0
        cis[1,]  <- NA
    }

    coeff           <- rbind( a, b )
    coef.names      <- c( "elevation", "slope" )
    coeff           <- data.frame( coeff, cis )
    names(coeff)    <- c( lab, "lower limit", "upper limit" )
    rownames(coeff) <- coef.names

	if (!is.null(data))detach(data)
	
    return(coeff)
}
