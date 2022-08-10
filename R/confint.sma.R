#' Returns confidence intervals for sma object
#'
#' @param object a sma object
#'
#' @return dataframe containing confidence interval
#' @export


confint.sma <- function(object){
  # Grab summary data
  grp_summary <- object$groupsummary
  
  # Function to create confint output
  produce_confint <- function(grp_summary){
    grp_summary %>% 
      dplyr::select(dplyr::ends_with("CI")) %>% 
      tidyr::pivot_longer(cols = 1:4) -> tmp
    
    tmp %>%
      dplyr::mutate(ci_type = ifelse(str_detect(tmp$name, "low"), "Lower", "Upper"),
             coef_type = ifelse(stringr::str_detect(tmp$name, "Int"), "(Intercept)", "slope")) %>% 
      tidyr::pivot_wider(id_cols = .data$coef_type, values_from = .data$value, names_from = .data$ci_type) %>% 
      as.data.frame() -> out
    
    rownames(out) <- out$coef_type
    
    dplyr::bind_rows(out[2,2:3], out[1,2:3])
  }
  
  # Is it multiple comparisons?
  if(length(grp_summary$group) > 1){
    # Split data by levels of group
    ls <- split(grp_summary, grp_summary$group)
    
    # For each list, apply the produce_confint function
    lapply(ls, produce_confint)
  }
  else(produce_confint(grp_summary))
}
