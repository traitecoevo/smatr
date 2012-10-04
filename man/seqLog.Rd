\name{seqLog}
\alias{seqLog}
\title{Sequence Generation
}
\description{
Generate multiplicative sequences, or series.
}
\usage{
seqLog(from, to, base = 10)
}
\arguments{
  \item{from,to}{the starting and (maximal) end value of a sequence.}
  \item{base}{multiplication value.}
  }
\details{
Starting at \code{from}, \code{seq} multiplies successively by \code{base} until the maximal value is reached. This is useful for generating tick-spacing on log-transformed axes. 
}

\examples{
#Sequence suitable for log base 10 labels
seqLog(1E-5, 1E5)

#Sequence suitable for log base 2 labels
seqLog(2, 128,base=2)
}
\seealso{
\code{\link{nicePlot}}, \code{\link{makeLogMinor}}
}

\keyword{misc}
