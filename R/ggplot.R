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
              "x" = ggplot_x(obj, pdat, ...),
              "y" = ggplot_y(obj, pdat, ...),
              "xy" = ggplot_xy(obj, pdat, ...))
  )
  p
}

#' Make ggplot with untransformed data
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab theme_bw

ggplot_none <- function(obj, pdat, ...){
  if(any(obj$groups == "all"))
  p <- ggplot(data = pdat, aes(x = X, y = line_Y), ...) 
  
  if(length(obj$groups) > 1)
  p <- ggplot(data = pdat, aes(x = X, y = line_Y, group = group, colour = group), ...)
  
  # Construct rest of plot
  p +
    geom_point(aes(x = X, y = Y, group = group, colour = group), shape = 21, size = 2) + 
    geom_line(...) + 
    ylab(label = obj$variables[1]) +
    xlab(label = obj$variables[2]) +
    theme_bw()
}

#' Make ggplot with transformed x axes only
#' @inheritParams ggplot.sma

ggplot_x <- function(obj, pdat, ...){
  if(any(obj$groups == "all"))
    p <- ggplot(data = pdat, aes(x = X_raw, y = line_Y), ...) 
    
  if(length(obj$groups) > 1)
   p <- ggplot(data = pdat, aes(x = X_raw, y = line_Y, group = group, colour = group), ...) 
    
  # Construct rest of plot
    p + 
    geom_point(aes(x = X_raw, y = Y, group = group, colour = group), shape = 21, size = 2) +
    geom_line(...) + 
    scale_x_log10() +
    ylab(label = obj$variables[1]) +
    xlab(label = obj$variables[2]) +
    theme_bw()
}

#' Make ggplot with transformed y axes only
#' @inheritParams ggplot.sma

ggplot_y <- function(obj, pdat, ...){
  if(any(obj$groups == "all"))
    p <- ggplot(data = pdat, aes(x = X, y = line_Y), ...) 
  
  if(length(obj$groups) > 1)
    p <- ggplot(data = pdat, aes(x = X, y = line_Y, group = group, colour = group), ...)
  
  # Construct rest of plot
  p +
    geom_point(aes(x = X, y = Y_raw, group = group, colour = group), shape = 21, size = 2) + 
    geom_line(...) + 
    scale_y_log10() + 
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
   if(any(obj$groups == "all")){
     # No groups
    pdat <- dplyr::tibble(X = obj$data[,2],
                   Y = obj$data[,1],
                   line_Y = summary(obj)$Int + X*summary(obj)$Slope) #a + x*B
   }else{
     groups <- levels(obj$data[,3])
     
     pdat <- dplyr::tibble(X = obj$data[,2],
                           Y = obj$data[,1],
                           group = obj$data[,3])
     
     for(i in 1:length(groups)){
       pdat[pdat$group == groups[i],"a"] <- get_coef(obj = obj, 
                                                   group_level = groups[i],
                                                   coef_type = "a")
       
       pdat[pdat$group == groups[i],"B"] <- get_coef(obj = obj, 
                                                   group_level = groups[i],
                                                   coef_type = "B")
       
       pdat <- pdat |> mutate(line_Y = a + X*B)
     }
   }
   pdat
 }
 
 #' Reformat data for plotting on log transformed x
 #'
 #' @inheritParams make_data_raw

 make_data_x <- function(obj){
   if(any(obj$groups == "all")){
    pdat <- dplyr::tibble(X = obj$data[,2], #On log scale
                   Y = obj$data[,1],
                   X_raw = 10^X,
                   line_Y = summary(obj)$Int + summary(obj)$Slope*log10(X_raw) #a+B*log10(x) 
     )
   }else{
     pdat <- dplyr::tibble(X = obj$data[,2],
                           Y = obj$data[,1],
                           X_raw = 10^X,
                           group = obj$data[,3])
     
     groups <- levels(obj$data[,3])
     
     for(i in 1:length(groups)){
       pdat[pdat$group == groups[i],"a"] <- get_coef(obj = obj, 
                                                     group_level = groups[i],
                                                     coef_type = "a")
       
       pdat[pdat$group == groups[i],"B"] <- get_coef(obj = obj, 
                                                     group_level = groups[i],
                                                     coef_type = "B")
       
       pdat <- pdat |> mutate(line_Y = a + B*log10(X_raw))
     }
   }
   pdat
 }
 

 #' Reformat data for plotting on log transformed y
 #'
 #' @inheritParams make_data_raw
 
 make_data_y <- function(obj){
   if(any(obj$groups == "all")){
     pdat <-  dplyr::tibble(X = obj$data[,2],
                            Y = obj$data[,1], #On log scale
                            Y_raw = 10^Y,
                            line_Y = 10^(summary(obj)$Int + summary(obj)$Slope*X) # 10^(a+x*B)
     )
   }else{
     pdat <- dplyr::tibble(X = obj$data[,2],
                           Y = obj$data[,1],
                           Y_raw = 10^Y,
                           group = obj$data[,3])
     
     groups <- levels(obj$data[,3])
     for(i in 1:length(groups)){
       pdat[pdat$group == groups[i],"a"] <- get_coef(obj = obj, 
                                                     group_level = groups[i],
                                                     coef_type = "a")
       
       pdat[pdat$group == groups[i],"B"] <- get_coef(obj = obj, 
                                                     group_level = groups[i],
                                                     coef_type = "B")
       
       pdat <- pdat |> mutate(line_Y = 10^(a + B*X)) # 10^(a+x*B)
     }
   }
   pdat
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


 #' Extract coefficients for each level of group
 #'
 #' @param obj object with sma class 
 #' @param group_level character string of group level
 #' @param coef_type type of coefficient you want a for intercept B for slope
 
 get_coef <- function(obj, group_level, coef_type){
   
   groupsum <- obj$groupsummary
   
   if(coef_type == "a"){
     a <- groupsum |> filter(group == group_level) |>  pull("Int")
     return(a)
   } 
   
   if(coef_type == "B"){
     B <- groupsum |> filter(group == group_level) |>  pull("Slope")
     return(B)
   }
 }
 