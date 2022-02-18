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
#' @return a list that has lcmem output corresponding with the random_vect provided.
#' @export


ran_can <- function(df, fixed, mixture, random_vect, subject, k = 5) {
  df_sym <- substitute(...(df = df))$df
  mos <- listenv::listenv()
  for (i in 1:length(random_vect)) {
    mos[[i]] %<-% lcmem(data = df, fixed = fixed, mixture = mixture, random = random_vect[[i]], subject = subject, k = k, df_sym = df_sym)
  }
  mos <- as.list(mos)
  return(mos)
}
