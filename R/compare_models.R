#' Compare models
#'
#' Outputs BIC and AIC for each model in the list provided
#' @param model_list List of lcmem outputs from ran_can function
#' @return data frame of with model number, model details, BIC and AIC other
#' model selection criteria. This function
#' also returns plots of the model selection criteria by model number.
#' @export
#' @example R\examples\compare_models.R

compare_models <- function(model_list) {
  tab <- lapply(1:length(model_list), function(i) {
    model <- model_list[[i]]
    sum_table <- lcmm::summarytable(model,
      model,
      which = c(
        "G",
        "loglik",
        "conv",
        "npm",
        "AIC",
        "BIC",
        "SABIC",
        "entropy",
        "ICL",
        "%class"
      ), display = FALSE
    )

    df_sum_table <- as.data.frame(sum_table)

    per_class_names <- names(df_sum_table)[grepl(x = names(df_sum_table),
                                                 pattern = "\\%class")]

    smallest_class = min(df_sum_table[, per_class_names])


    a <- df_sum_table[1, ]
    vect <- c(
      "Model Number" = i,
      "Model" = paste0(
        "Random = ",
        paste(model$call$random, collapse = ""),
        "; idiag = ",
        paste(model$call$idiag, collapse = ""),
        "; nwg = ",
        paste(model$call$nwg, collapse = "")
      ),
      "k" = a$G,
      "conv" = a$conv,
      "npm" = a$npm,
      "loglik" = round(a$loglik),
      "BIC" = round(a$BIC),
      "AIC" = round(a$AIC),
      "entropy" = round(a$entropy, digits = 5),
      "ICL" = round(a$ICL),
      "Smallest Class (%)" = smallest_class
    )

    return(vect)
  })

  tab <- as.data.frame(do.call(rbind, tab))
  vars <- c("k",
            "conv",
            "npm",
            "loglik",
            "BIC",
            "AIC",
            "entropy",
            "ICL",
            "Smallest Class (%)")
  tab[, vars] <- lapply(vars, function(var) as.numeric(tab[, var]))
  plot <- tidyr::pivot_longer(tab,
                              c(BIC, entropy, ICL, conv, k, `Smallest Class (%)`),
                              names_to = "mo_crit") %>%
    ggplot(aes(x = factor(as.integer(`Model Number`)))) +
    geom_point(aes(y = value)) +
    geom_line(aes(y = value, group = "None")) +
    facet_wrap(vars(mo_crit), ncol = 3, scales = "free") +
    labs(x = "Model Number", title = "Model Criteria")
  return(list(plot = plot, table = tab))
}
