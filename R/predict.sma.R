#' Predicting from an (Standardized) major axis estimation
#'
#' @param object sma obj
#' @param ... arguments passed
#'
#' @return A cautionary message
#' @export

predict.sma <- function(object,...){
  message("MA and SMA were not designed for prediction of Y nor X. Use fitted() if you must...")  
}