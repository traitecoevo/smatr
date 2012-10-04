\name{multcompmatrix}
\alias{multcompmatrix}
\title{
Multiple comparisons graphical matrix
}
\description{
Print a matrix of pair-wise comparisons based on an 'sma' fit. 
}
\usage{
multcompmatrix(smfit, sort = TRUE)
}

\arguments{
  \item{smfit}{
An object of class 'sma', fit with \code{\link{sma}}.
}
  \item{sort}{
Logical. Specifies whether or not to sort groups from smallest to largest value based on the parameter of interest (slope, elevation or mean fitted value).}
}
\details{
A matrix of comparisons is drawn, based on the P values of the pair-wise tests between levels of the grouping variable. An 'X' indicates P < 0.05, 'o' indicates 0.05 < P < 0.1.
}
\value{
Invisibly returns the (character) matrix.
}
\author{
Remko Duursma and Daniel Falster.
}

\seealso{
\code{\link{sma}}
}
\examples{

# Print the matrix of comparisons:
data(leaflife)
sm1 <- sma(lma ~ longev + site, data=leaflife, multcomp=TRUE)
multcompmatrix(sm1)

# Write the matrix to a file like this:
\dontrun{
capture.output(multcompmatrix(sm1), file="sm1matrix.txt")
}

}
\keyword{misc}

