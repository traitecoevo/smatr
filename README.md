 <!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/smatr)](https://CRAN.R-project.org/package=smatr)

  [![Travis build status](https://travis-ci.com/fontikar/smatr3.svg?branch=master)](https://travis-ci.com/fontikar/smatr3)
  <!-- badges: end -->

# smatr: (Standardised) Major Axis Estimation and Testing Routines


`smatr` is an R package for fitting (Standardised) Major Axis Lines to bivariate data. The package is described in the following publication:

Warton, David I., Duursma, Remko A., Falster, Daniel S. and Taskinen,
  Sara (2012) **smatr 3 - an R package for estimation and inference about
  allometric lines**. Methods in Ecology and Evolution, 3(2), 257-259. doi:[10.1111/j.2041-210X.2011.00153.x](http://doi.org/10.1111/j.2041-210X.2011.00153.x).


**Abstract:**

1. The Standardised Major Axis Tests and Routines (SMATR) software provides tools for estimation and inference about allometric lines, currently widely used in ecology and evolution.
2. This paper describes some significant improvements to the functionality of the package, now available on R in smatr version 3.
3. New inclusions in the package include sma and ma functions that accept formula input and perform the key inference tasks; multiple comparisons; graphical methods for visualising data and checking (S)MA assumptions; robust (S)MA estimation and inference tools.

The package was programmed by [David Warton](http://web.maths.unsw.edu.au/~dwarton/), [Remko Duursma](http://www.remkoduursma.com) and [Daniel Falster](http://danielfalster.com) and is maintained by Remko Duursma [here](https://bitbucket.org/remkoduursma/smatr/).


## Installation

You can simply install this package from CRAN,

```R
install.packages("smatr")
```

Or install the development version from bitbucket,

```R
devtools::install_bitbucket ("smatr", "remkoduursma")
```


## Meta

Please report any [issues or bugs](https://bitbucket.org/remkoduursma/smatr/issues).

To cite package `smatr` in publications use:

```coffee
   Warton, David I., Duursma, Remko A., Falster, Daniel S. and Taskinen,
  Sara (2012) smatr 3 - an R package for estimation and inference about
  allometric lines Methods in Ecology and Evolution, 3(2), 257-259.  doi:10.1111/j.2041-210X.2011.00153.x. 
```

Get citation information for `smatr` in R with `citation(package = 'smatr')`.


