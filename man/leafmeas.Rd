\name{leafmeas}

\docType{data}

\alias{leafmeas}

\title{Leaf mass per area and photosynthetic rate for plant species from different sites}

\description{This dataset contains the leaf mass per area (LMA) and photosynthetic rate per meaf mass (Amass) of individual plants from 81 plant species at four sites in south-eastern Australia. Sites represent contrasts along rainfall and soil phosphorous gradients.}

\usage{
    data(leafmeas)
}

\format{A data frame containing 529 rows and 4 variables:
   \describe{
    \item{site}{(factor) The site at which this species was sampled (coded as Murrua, WH, RHM, RHW)}

    \item{spp}{(factor) Species of the observed plant species}

    \item{Amass}{(numeric) Photosynthetic rate per leaf mass ()}

    \item{lma}{(numeric) Leaf mass per area (m2/kg)}
   }
}

\source{
Wright I. J., Reich P. B. and Westoby M. (2001) Strategy shifts in leaf physiology, structure and nutrient content between species of high- and low-rainfall and high- and low-nutrient habitats.  \emph{Functional Ecology} \bold{15}, 423--434.
}

\keyword{datasets}
