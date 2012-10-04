\name{leaflife}

\docType{data}

\alias{leaflife}

\title{Leaf longevity and leaf mass per area for plant species from different sites}

\description{This dataset contains the leaf longevity and leaf mass per area of 75 plant species from four sites in south-eastern Australia. Sites represent contrasts along rainfall and soil phosphorous gradients.}

\usage{
    data(leaflife)
}

\format{A data frame containing 75 rows and 5 variables:
   \describe{
    \item{site}{the site at which this species was sampled (coded as 1, 2, 3, or 4)}

    \item{rain}{The level of annual rainfall at the site (coded as "high" or "low")}

    \item{soilp}{The level of soil phosphate concentration at the site (coded as "high" or "low")}

    \item{longev}{Leaf longevity (years)}

    \item{lma}{Leaf mass per area (m2/kg)}
   }
}

\source{
Wright I. J., Westoby M. and Reich P. B. (2002) Convergence towards higher leaf mass per area in dry and nutrient-poor habitats has different consequences for leaf life span.  \emph{Journal of Ecology} \bold{90}, 534--543.
}

\keyword{datasets}
