#' Evaluate models
#'
#' Outputs BIC and AIC for each model in the list provided
#' @param list List of lcmem outputs from ran_can function
#' @return data frame of with model number, model details, BIC and AIC
#' @export

mo_eval <- function(list){
  as.data.frame(do.call(rbind, lapply(1:length(list), function(i){
    mo <- list[[i]]
    c("Model Number" = i, "Model" = paste0("k = ", mo$parameters$k, "; Random = ", mo$parameters$random,"; idiag = ", mo$parameters$idiag,"; nwg = ", mo$parameters$nwg), "BIC" = mo$model$BIC, "AIC" = mo$model$AIC)
  } )))
}
