#' Multiple comparisons graphical matrix
#' 
#' @description Print a matrix of pair-wise comparisons based on an 'sma' fit.
#' 
#' A matrix of comparisons is drawn, based on the P values of the pair-wise
#' tests between levels of the grouping variable. An 'X' indicates P < 0.05,
#' 'o' indicates 0.05 < P < 0.1.
#' 
#' @param smfit An object of class 'sma', fit with \code{\link{sma}}.
#' @param sort Logical. Specifies whether or not to sort groups from smallest
#' to largest value based on the parameter of interest (slope, elevation or
#' mean fitted value).
#' @return Invisibly returns the (character) matrix.
#' @author Remko Duursma and Daniel Falster.
#' @seealso \code{\link{sma}}
#' @keywords misc
#' @export
#' @examples
#' 
#' 
#' # Print the matrix of comparisons:
#' data(leaflife)
#' sm1 <- sma(lma ~ longev + site, data=leaflife, multcomp=TRUE)
#' multcompmatrix(sm1)
#' 
#' # Write the matrix to a file like this:
#' \dontrun{
#' capture.output(multcompmatrix(sm1), file="sm1matrix.txt")
#' }
#' 
#' 
multcompmatrix <- function(smfit, sort=TRUE){
	
	if(smfit$multcompdone == "none")stop("Call sma() with multcomp=TRUE to use this function")
	
	ngr <- length(smfit$groups)
	x <- smfit$multcompresult
	m <- matrix(nrow=ngr, ncol=ngr)
	cn <- combn(1:ngr,2)

	for(i in 1:ncol(cn)){
		m[cn[1,i],cn[2,i]] <- x$Pval[i]
		m[cn[2,i],cn[1,i]] <- x$Pval[i]
	}

	m2 <- matrix(rep("",ngr^2), nrow=ngr)
	diag(m2) <- "-"
	m2[m < 0.1] <- ".  "
	m2[m < 0.05] <- "*  "
	m2[m < 0.01] <- "** "
	m2[m < 0.001] <- "***"
	l <- list(x=smfit$groups, y=smfit$groups)
	names(l) <- names(x)[1:2]
	dimnames(m2) <- l

	if(sort){
		neworder <- c(0, order(tapply(x[,6], x[,1], mean))) + 1
		m2 <- m2[neworder,neworder]
	}
	
	cat("Multiple comparison matrix.\n")
	cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
	print.table(m2, justify="left")
return(invisible(m2))
}
