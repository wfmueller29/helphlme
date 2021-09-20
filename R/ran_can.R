#' Random Effects Candidates for Latent Class Mixed Effects Models
#'
#' This function applies lcmem across a vector of random effects candidates and also allows
#' the option to parallelize with the futures package
#' @param df data frame object with data for models
#' @param fixed a string that represents a two-side linear formual object for the fixed effects in a linear mixed model.
#'   By default, an intercept is included. If no intercept, -1 should be the first term included on the right of ~.
#' @param mixture a string that represents one-sided formula object for the class-specific fixed effects in the linear mixed model
#'   (to specify only for a number of latent classes greater than 1). Among the list of covariates included in fixed,
#'   the covariates with class-specific regression parameters are entered in mixture separated by +. By default, an intercept is included.
#'   If no intercept, -1 should be the first term included.
#' @param random_vect a character vector where each element represents an optional one-sided formula for the random-effects in the linear mixed model. Covariates
#'   with a random-effect are separated by +. By default, an intercept is included. If no intercept, -1 should be the first term included.
#' @param subject name of the covariate representing the grouping structure specified with ''.
#' @param k number of classes
#' @param par boolean argument specifying if models should be run in parallel
#' @return a list that has lcmem output corresponding with the random_vect provided.
#' @export


ran_can <- function(df, fixed, mixture, random_vect, subject, k = 5, par = F){
  df <- substitute(...(df = df))$df

  if(par ==F){
    ran_can_base(df, fixed, mixture, random_vect, subject, k)
  } else{
    ran_can_par(df, fixed, mixture, random_vect, subject, k)
  }
}

ran_can_base <- function(df, fixed, mixture, random_vect, subject, k){
  mos <- lapply(1: length(random_vect), function(i){
    mo <- lcmem(df = df, fixed = fixed, mixture = mixture, random = random_vect[[i]], subject = subject, k = k)
  })
  return(mos)
}


ran_can_par <- function(df, fixed, mixture, random_vect, subject, k){
  mos <- listenv::listenv()
  for(i in length(forms)){
    mos[[i]] %<-% lcmem(df = df, fixed = fixed, mixture = mixture, random = random_vect[[i]], subject = subject, k = k)
  }
  mos <- as.list(mos)
  return(mos)
}
