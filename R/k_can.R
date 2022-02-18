#' Class Number Candidates for Latent Class Mixed Effects Models
#'
#' This function applies lcmem across a vector of class candidates defined by (1:max_k)
#' and also allows the option to parallelize with the futures package
#' @param df data frame object with data for models
#' @param fixed a string that represents a two-side linear formual object for the fixed effects in a linear mixed model.
#'   By default, an intercept is included. If no intercept, -1 should be the first term included on the right of ~.
#' @param mixture a string that represents one-sided formula object for the class-specific fixed effects in the linear mixed model.
#'   Among the list of covariates included in fixed,
#'   the covariates with class-specific regression parameters are entered in mixture separated by +. By default, an intercept is included.
#'   If no intercept, -1 should be the first term included.
#' @param random a string that represents an optional one-sided formula for the random-effects in the linear mixed model. Covariates
#'   with a random-effect are separated by +. By default, an intercept is included. If no intercept, -1 should be the first term included.
#' @param subject name of the covariate representing the grouping structure specified with ''.
#' @param max_k the number of classes to apply the model structure
#' @param idiag optional logical for the structure of the variance-covariance matrix of the random-effects.
#'   If FALSE, a non structured matrix of variance-covariance is considered (by default).
#'   If TRUE a diagonal matrix of variance-covariance is considered.
#' @param nwg optional logical indicating if the variance-covariance of the random-effects is class-specific.
#'   If FALSE the variance-covariance matrix is common over latent classes (by default).
#'   If TRUE a class-specific proportional parameter multiplies the variance-covariance matrix in each class
#'   (the proportional parameter in the last latent class equals 1 to ensure identifiability).
#' @return a list that has lcmem output corresponding with the vector (1:max_k) provided.
#' @importFrom future %<-%
#' @export

k_can <- function(df, fixed, mixture, random, subject, max_k, idiag = FALSE, nwg = FALSE) {
  df_sym <- substitute(...(df = df))$df
  mos <- listenv::listenv()
  mos[[1]] <- lcmem(data = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = 1, df_sym = df_sym, idiag = idiag)
  betas <- mos[[1]]$model
  for (i in 2:max_k) {
    mos[[i]] %<-% lcmem(data = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = i, B = betas, df_sym = df_sym, idiag = idiag, nwg = nwg)
  }
  mos <- as.list(mos)
  return(mos)
}
