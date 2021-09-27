#' Class Number Candidates for Latent Class Mixed Effects Models
#'
#' This function applies lcmem across a vector of class candidates defined by (1:max_k)
#' and also allows the option to parallelize with the futures package
#' @param df data frame object with data for models
#' @param fixed a string that represents a two-side linear formual object for the fixed effects in a linear mixed model.
#'   By default, an intercept is included. If no intercept, -1 should be the first term included on the right of ~.
#' @param mixture a string that represents one-sided formula object for the class-specific fixed effects in the linear mixed model
#'   (to specify only for a number of latent classes greater than 1). Among the list of covariates included in fixed,
#'   the covariates with class-specific regression parameters are entered in mixture separated by +. By default, an intercept is included.
#'   If no intercept, -1 should be the first term included.
#' @param random a string that represents an optional one-sided formula for the random-effects in the linear mixed model. Covariates
#'   with a random-effect are separated by +. By default, an intercept is included. If no intercept, -1 should be the first term included.
#' @param subject name of the covariate representing the grouping structure specified with ''.
#' @param max_k the number of classes to apply the model structure
#' @return a list that has lcmem output corresponding with the vector (1:max_k) provided.
#' @export

k_can <- function(df, fixed, mixture, random, subject, max_k){
  df_sym <- substitute(...(df = df))$df
  mos <- listenv::listenv()
  mos[[1]] <- lcmem(data = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = 1, df_sym = df_sym)
  betas <- mos[[1]]$model
  for(i in 2:max_k){
    mos[[i]] %<-% lcmem(data = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = i, B = betas, df_sym = df_sym)

  }
  mos <- as.list(mos)
  return(mos)
}
