# Make the data for plotting

#Get fitted objects
# none <- sma(longev ~ lma, data=leaflife)
# x_only <- sma(longev ~ lma, log='x',  data=leaflife)
# y_only <- sma(longev ~ lma, log='y', data=leaflife)
# both_xy <- sma(longev ~ lma, log='xy',  data=leaflife)



 # make_plot_data <- function(obj){
 #   dat <- switch(obj$log,
 #                  "" = make_data_raw(obj),
 #                  "x" = make_data_x(obj),
 #                  "y" = make_data_y(obj),
 #                  "xy" = make_data_xy(obj)
 #   )
 # }
   
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
   
   # make_data_y(obj)<- function(obj){
   #   dplyr::tibble(X = obj$data[,2], 
   #                 Y = obj$data[,1], #On log scale
   #                 Y_raw = 10^Y,
   #                 line_Y = 10^(summary(y_only)$Int + summary(y_only)$Slope*X)) #10^(a+x*B)
   # }
   
   # make_data_xy(obj)<- function(obj){
   #   dplyr::tibble(X = obj$data[,2], #On log scale
   #                 Y = obj$data[,1], #On log scale
   #                 X_raw = 10^X,
   #                 Y_raw = 10^Y,
   #                 line_Y = 10^(summary(both_obj)$Int + summary(both_obj)$Slope*X)) #10^a*x^B 
   # }

 
 
 # plot_sma <_ function(obj){
 #   switch(obj$log,
 #          "" = plot_sma_raw(obj, ...)
 #          "x" = ,
 #          "y" = ,
 #          "xy" = )
 #          
 #   
 #   plot_sma_raw <- function(obj, ...){
 #     
 #   }
 #   
 #   
 # 
 # }
   
   # 
   # library(ggplot2)
   # ggplot(data = raw_plot_df, aes(x = X, y = line_Y)) + 
   #   geom_line() + 
   #   theme_bw() + 
   #   theme()
