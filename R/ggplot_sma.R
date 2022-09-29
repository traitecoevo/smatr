#' Plot sma obj using ggplot2

make_plot_data <- function(obj){
  
  if(obj$log == "") 
    make_data_raw(obj) 
  
  else(
  dat <- switch(obj$log,
                "x" = make_data_x(obj),
                "y" = make_data_y(obj),
                "xy" = make_data_xy(obj)
                )
  )
}
   
#' Reformat data for plotting
#'
#' @param obj ma or sma obj
#'
#' @return tibble of plotting data
#' @keywords internal

 make_data_raw <- function(obj){
   dplyr::tibble(X = obj$data[,2],
                 Y = obj$data[,1],
                 line_Y = summary(obj)$Int + X*summary(obj)$Slope) #a + x*B
 }
 
 # make_data_x(obj) <- function(obj){
 #   dplyr::tibble(X = obj$data[,2], #On log scale
 #                 Y = obj$data[,1],
 #                 X_raw = 10^X,
 #                 line_Y = summary(x_only)$Int + summary(x_only)$Slope*X #a+B*log10(x)
 #   )
 # }
 # 
 # make_data_y(obj)<- function(obj){
 #   dplyr::tibble(X = obj$data[,2],
 #                 Y = obj$data[,1], #On log scale
 #                 Y_raw = 10^Y,
 #                 line_Y = 10^(summary(y_only)$Int + summary(y_only)$Slope*X)) #10^(a+x*B)
 # }
 # 
 # make_data_xy(obj)<- function(obj){
 #   dplyr::tibble(X = obj$data[,2], #On log scale
 #                 Y = obj$data[,1], #On log scale
 #                 X_raw = 10^X,
 #                 Y_raw = 10^Y,
 #                 line_Y = 10^(summary(both_obj)$Int + summary(both_obj)$Slope*X)) #10^a*x^B
 # }
 
 
