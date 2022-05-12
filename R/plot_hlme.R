#' Plot the trajectories of a an hlme object
#'
#' Provide an hlme model, this function will use the base plot function to plot the trajectories
#' determined by the fixed effects of the model
#' @param df data frame used to predict model outcomes. Should be prodcued by 
#' create_pred_df function
#' @param model hlme model output. If modelled using lcmem, use lcmem$model.
#' @param age string specfiying age variable name
#' @param ... arguements to pass to the plot function (eg. main = "title of plot")
#' @return outputs base plot object. This object cannot be assigned to an R object.
#' @export


plot_hlme <- function(df, model, age, ...) {
  if (model$conv == 1 | model$conv == 2) {
    pred1 <- lcmm::predictY(model, df, var.time = paste0(age, "_ns"))

    plot(pred1, ...)
  } else {
    sub <- paste0(
      "k = ", model$call$ng, "; Random = ", deparse(model$call$random),
      "; idiag = ", model$call$idiag, "; nwg = ", model$call$nwg
    )
    message(sub, "did not converge")
  }
}

