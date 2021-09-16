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
#' @param par boolean argument specifying if models should be run in parallel
#' @return a list that has lcmem output corresponding with the vector (1:max_k) provided.
#' @export

k_can <- function(df, fixed, mixture, random, subject, max_k, par = F){
  df <- substitute(...(df = df))$df

  if(par==F){
    k_can_base(df, fixed, mixture, random, subject, max_k)
  } else{
    k_can_par(df, fixed, mixture, random, subject, max_k)
  }
}

k_can_base <- function(df, fixed, mixture,  random,subject, max_k){
  mos <- list()
  mos[[1]] <- lcmem(df = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = 1)
  betas <- mos[[1]]$model
  mos <- c(mos, lapply(2:max_k, function(i){
    mo <- lcmem(df = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = i, B = betas)
  }))
  return(mos)
}

k_can_par <- function(df, fixed, mixture, random, subject, max_k){
  mos <- listenv()
  mos[[1]] <- lcmem(df = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = 1)
  betas <- mos[[1]]$model
  for(i in 2:max_k){
    mos[[i]] %<-% lcmem(df = df, fixed = fixed, mixture = mixture, random = random, subject = subject, k = i, B = betas)

  }
  mos <- as.list(mos)
  return(mos)
}
