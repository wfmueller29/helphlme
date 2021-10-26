#' Evaluate models
#'
#' Outputs BIC and AIC for each model in the list provided
#' @param list List of lcmem outputs from ran_can function
#' @return data frame of with model number, model details, BIC and AIC other model selection criteria. This function
#' also returns plots of the model selection criteria by model number.
#' @export

mo_eval <- function(list){
  tab <- as.data.frame(do.call(rbind, lapply(1:length(list), function(i){
    mo <- list[[i]]
    a <- as.data.frame(summarytable(mo$model, mo$model, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"), display = FALSE))
    a <- a[1,]
    vect <- c("Model Number" = i, "Model" = paste0( "Random = ", mo$parameters$random,"; idiag = ", mo$parameters$idiag,"; nwg = ", mo$parameters$nwg),
      "k" = a$G,
      "conv" = a$conv,
      "npm" = a$npm,
      "loglik" = round(a$loglik),
      "BIC" = round(a$BIC),
      "AIC" = round(a$AIC),
      "entropy" = round(a$entropy, digits = 5),
      "ICL" = round(a$ICL))
    return(vect)
  } )))
  plot <- pivot_longer(tab, npm:ICL, names_to = "mo_crit") %>%
    ggplot(aes(x = factor(as.integer(`Model Number`)))) +
    geom_point(aes(y = value)) +
    geom_line(aes(y =value, group = "None")) +
    facet_wrap(vars(mo_crit), ncol = 3, scales = "fixed") +
    labs(x = "Model Number", title = "Model Criteria")
  return(list(plot = plot, table = tab))
}
