#' Title
#'
#' @param obj 
#' @param ... arguments passed to ggplot()
#'
#' @return ggplot2 obj
#' @export
#'
#' 
ggplot.sma <- function(obj, ...){
  pdat <- make_plot_data(obj)
  
  if(obj$log == "")
  p <- ggplot_none(obj, pdat, ...)
  else(
  p <- switch(obj$log,
              "x" = ggplot_x(obj, ...),
              "y" = ggplot_y(obj, ...),
              "xy" = ggplot_xy(obj, ...))
  )
  p
}

#' Make ggplot with untransformed data
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab theme_bw

ggplot_none <- function(obj, pdat, grp = NULL, ...){
  ggplot(data = pdat, aes(x = X, y = line_Y, group = grp), ...) + 
    geom_line(...) + 
    ylab(label = obj$variables[1]) +
    xlab(label = obj$variables[2]) +
    theme_bw()
}

#' Make ggplot with transformed x axes only
#' @inheritParams ggplot.sma

ggplot_x <- function(obj, pdat, ...){
  ggplot(data = pdat, aes(x = X_raw, y = line_Y), ...) + 
    geom_line(...) + 
    ylab(label = obj$variables[1]) +
    xlab(label = obj$variables[2]) +
    theme_bw()
}


#' Make plotting data from sma obj 
#'
#' @param obj 

make_plot_data <- function(obj){
  
  if(obj$log == "") 
  pdat  <- make_data_raw(obj) 
  
  else(
    pdat <- switch(obj$log,
                "x" = make_data_x(obj),
                "y" = make_data_y(obj),
                "xy" = make_data_xy(obj)
                )
  )
  pdat
}
   
#' Reformat data for plotting on untransformed data
#'
#' @param obj ma or sma obj
#' @importFrom dplyr group_by mutate
#' @return tibble of plotting data
#' @keywords internal

 make_data_raw <- function(obj){
   if(obj$gt == "none"){
     # No groups
    pdat <- dplyr::tibble(X = obj$data[,2],
                   Y = obj$data[,1],
                   line_Y = summary(obj)$Int + X*summary(obj)$Slope) #a + x*B
   }else{
     groups <- levels(obj$data[,3])
     pdat <- dplyr::tibble(X = obj$data[,2],
                           Y = obj$data[,1],
                           group = obj$data[,3])
   }
 }
 
 #' Reformat data for plotting on log transformed x
 #'
 #' @inheritParams make_data_raw

 make_data_x <- function(obj){
   dplyr::tibble(X = obj$data[,2], #On log scale
                 Y = obj$data[,1],
                 X_raw = 10^X,
                 line_Y = summary(obj)$Int + summary(obj)$Slope*log10(X_raw) #a+B*log10(x) 
   )
 }

 #' Reformat data for plotting on log transformed y
 #'
 #' @inheritParams make_data_raw
 
 make_data_y <- function(obj){
   dplyr::tibble(X = obj$data[,2],
                 Y = obj$data[,1], #On log scale
                 line_Y = 10^(summary(obj)$Int + summary(obj)$Slope*X) # 10^(a+x*B)
   )
 }

 #' Reformat data for plotting on log transformed x and y
 #'
 #' @inheritParams make_data_raw
 
 make_data_xy <- function(obj){
   dplyr::tibble(X = obj$data[,2], #On log scale
                 Y = obj$data[,1], #On log scale
                 X_raw = 10^X,
                 line_Y = 10^summary(obj)$Int * X_raw^summary(obj)$Slope # 10^a*x^B
                 )
 }


