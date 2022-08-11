\name{makeLogMinor}
\alias{makeLogMinor}
\title{Generate spacing for minor tick marks on a plot with log-scaled axes
}
\description{
Generates a sequence of numbers providing minor tick marks on log scaled axes. 
}
\usage{
makeLogMinor(major)
}
\arguments{
  \item{major}{a vector of values giving major tick marks.
  }
}
\details{
A vector is created according to the following algorithm. For each pair of adjacent values (x1, x2) in \code{major}, the function adds the values (x1, 2*x1, 3*x1, ..., x2) to the vector of return values. 

This is useful for generating spacing of minor tick-values on log-transformed axes. 
}

\examples{
#Sequence suitable for log base 10 labels
makeLogMinor(seqLog(1E-5, 1E5))
}
\seealso{
\code{\link{nicePlot}}, \code{\link{seqLog}}
}

\keyword{misc}
