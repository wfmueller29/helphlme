#' Evaluate ran_can models
#'
#' Outputs BIC and AIC for each model in the list provided
#' @param list List of lcmem outputs from ran_can function
#' @return data frame of with model number, model details, BIC and AIC
#' @export

ran_can_eval <- function(list){
  as.data.frame(do.call(rbind, lapply(1:length(list), function(i){
    mo <- list[[i]]
    c("Model Number" = i, "Model" = paste0("Random = ", mo$parameters$random), "BIC" = mo$model$BIC, "AIC" = mo$model$AIC)
  } )))
}

#' Evaluate k_can models
#'
#' Outputs BIC and AIC for each model in the list provided
#' @param list List of lcmem outputs from k_can function
#' @return data frame of with k, BIC and AIC
#' @export


k_can_eval <- function(list){
  as.data.frame(do.call(rbind, lapply(list, function(mo) c("k" = mo$parameters$k, "BIC" = mo$model$BIC, "AIC" = mo$model$AIC))))
}

#' Evaluate ran_refine models
#'
#' Outputs BIC and AIC for each model in the list provided
#' @param list List of lcmem outputs from ran_refine function
#' @return data frame of with model number, model details, BIC and AIC
#' @export


ran_refine_eval <- function(list){
  as.data.frame(do.call(rbind, lapply(1:length(list), function(i){
    mo <- list[[i]]
    c("Model Number" = i, "Model" = paste0("Random = ", mo$parameters$random,"; idiag = ", mo$parameters$idiag,"; nwg = ", mo$parameters$nwg), "BIC" = mo$model$BIC, "AIC" = mo$model$AIC)
  } )))
}
