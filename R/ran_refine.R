#' Random Effects Refinement for Latent Class Mixed Effects Models
#'
#' This function applies lcmem across a vector of random effects candidates.
#' Each element is applied for the four comibination of nwg = T/F and idiag = T/F.
#' It also allows the option to parallelize with the futures package
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
#' @param k integer specfiyin number of classes
#' @return a list that has lcmem output.
#' @export

ran_refine <- function(df, fixed, mixture, random_vect, subject, k, par = F){
  df_sym <- substitute(...(df = df))$df
  betas_if <- listenv::listenv()
  for(i in 1:length(random_vect)){
    betas_if[[i]] %<-% lcmem(data = df, fixed = fixed, mixture = mixture, random = random_vect[[i]], subject = subject, k = 1, df_sym = df_sym)$model
  }
  betas_it <- listenv::listenv()
  for(i in 1:length(random_vect)){
    betas_it[[i]] %<-% lcmem(data = df, fixed = fixed, mixture = mixture, random = random_vect[[i]], subject = subject, k = 1, idiag = TRUE, df_sym = df_sym)$model
  }
  betas_if <- as.list(betas_if)
  betas_it <- as.list(betas_it)
  mos <- listenv::listenv()
  for(i in 1:length(random_vect)){
    j <- 0
    for(idiag in c(TRUE, FALSE)){
      for(nwg in c(TRUE,FALSE)){
        if(idiag){beta <- betas_it}
        else{beta <- betas_if}
        mos[[i + (j)*length(random_vect)]] %<-% lcmem(data = df, fixed = fixed, mixture = mixture, random = random_vect[[i]], subject = subject, k = k, B = beta[[i]], idiag = idiag, nwg = nwg, df_sym = df_sym)
        j <- j + 1
      }
    }
  }
  mos <- as.list(mos)
  return(mos)
}

