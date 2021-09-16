#' Plot the trajectories of a an hlme object
#'
#' Provide an hlme model, this function will use the base plot function to plot the trajectories
#' determined by the fixed effects of the model
#' @param df data frame used to model the data. Should be created by lcmem_prep function
#' @param model hlme model output. If modelled using lcmem, use lcmem$model.
#' @param age string specfiying age variable name
#' @param fixcov character vector specifiying other fixed effects covariates modelled besides age
#' @param main main title for plot
#' @param xlab xlabel for plot
#' @param ylab for plot
#' @param sub subtitle for plot
#' @return outputs base plot object. This object cannot be assigned to an R object.
#' @export


lcplot <- function(df, model, age, fixcov = NULL, main = "", xlab = "", ylab = "", sub = ""){
  age2 <- paste0(age,"2")
  # sequence all values of age from mininum to maximum age
  vect <- seq(min(df[,paste0(age,"_ns")]), max(df[,paste0(age,"_ns")]),length = 50)
  df_pred <- data.frame(vect)
  names(df_pred) <- age
  df_pred[,age2] <- df_pred[,age]*df_pred[,age]
  df_pred[,fixcov] <- lapply(1:length(fixcov), function(i){ ##sex all factor covariates = 0
    fixcov <- 0
  })
  df_pred_scale <- lcmem_prep(df_pred, c(age, age2)) # use lcmem_prep on predict dataframe
  df_pred_scale <- df_pred_scale

  pred1 <- lcmm::predictY(model, df_pred_scale, var.time = paste0(age,"_ns"))

  plot(pred1, main = main, xlab = xlab, ylab = ylab, sub  = sub)

}

#' Plot the trajectories of a list of lcmem function outputs
#'
#' Provided a list of lcmem function outputs, this function will use the base plot function
#' to plot the trajectories determined by the fixed effects of the model. Subtitle of the plots
#' will be automatically determined by k and random effects structure
#' @param df data frame used to model the data. Should be created by lcmem_prep function
#' @param model_list List of lcmem outputs.
#' @param age string specfiying age variable name
#' @param fixcov character vector specifiying other fixed effects covariates modelled besides age
#' @param main main title for plot
#' @param xlab xlabel for plot
#' @param ylab for plot
#' @return outputs base plot objects for each model provided.
#' @export

lcplot_apply <- function(df, model_list, age, fixcov = NULL, main = "", xlab = "", ylab = ""){
  for(i in 1:length(model_list)){
    sub <- paste0("k = ", model_list[[i]]$parameters$k, "; Random = ", model_list[[i]]$parameters$random,
                  "; idiag = ", model_list[[i]]$parameters$idiag, "; nwg = ", model_list[[i]]$parameters$nwg)
    lcplot(df = df,
           model = model_list[[i]]$model,
           age = age,
           fixcov = fixcov,
           main = main,
           xlab = xlab,
           ylab = ylab,
           sub = sub)
  }
}
