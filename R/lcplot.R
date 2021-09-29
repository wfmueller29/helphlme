#' Plot the trajectories of a an hlme object
#'
#' Provide an hlme model, this function will use the base plot function to plot the trajectories
#' determined by the fixed effects of the model
#' @param df data frame used to predict model outcomes. Should be prodcued by lcpred
#' @param model hlme model output. If modelled using lcmem, use lcmem$model.
#' @param age string specfiying age variable name
#' @param ... arguements to pass to the plot function (eg. main = "title of plot")
#' @return outputs base plot object. This object cannot be assigned to an R object.
#' @export


lcplot <- function(df, model, age, ...){

  if(model$conv == 1 | model$conv == 2){
    pred1 <- lcmm::predictY(model, df, var.time = paste0(age,"_ns"))

    plot(pred1, ...)
  } else{
    sub <- paste0("k = ", model$call$ng, "; Random = ", deparse(model$call$random),
                  "; idiag = ", model$call$idiag, "; nwg = ", model$call$nwg)
    message(sub, "did not converge")
  }


}

#' Create Dataframe to Predict hlme Outcome
#'
#' This funciton creates dataframes that are used by the predictY function in the lcmm package to plot hlme models
#'
#' @param df dataframe create used to model the data. Should be created by lcmem_prep fucntion
#' @param age string of age variable used in the model
#' @param square string of quadratic age variable if a quadratic term was include
#' @param cube character string of cubic age variable if cubic variable included
#' @param fixcov character vector of other factor covariates included in the model (eg. sex and strain)
#' @param center was data centered T or F
#' @param scale was data scale T or F
#' @return a dataframe
#' @export

lcpred <- function(df, age, square = NULL, cube = NULL, fixcov = NULL, center = FALSE, scale = FALSE){
  vect <- seq(min(df[,paste0(age,"_ns")]), max(df[,paste0(age,"_ns")]),length = 50)
  df_pred <- data.frame(vect)
  names(df_pred) <- age
  vars <- age
  if(!is.null(square)){
    df_pred[,square] <- df_pred[,age]*df_pred[,age]
    vars <- c(vars, square)
  }
  if(!is.null(cube)){
    df_pred[,cube] <- df_pred[,age]*df_pred[,age]*df_pred[,age]
    vars <- c(vars, cube)

  }
  df_pred[,fixcov] <- lapply(1:length(fixcov), function(i){ ##sex all factor covariates = 0
    fixcov <- 0
  })
  df_pred <- lcmem_prep(df = df_pred, vars = vars, center = center, scale = scale) # use lcmem_prep on predict dataframe
  df_pred <- df_pred
  return(df_pred)

}

