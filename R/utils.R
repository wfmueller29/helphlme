#' Sample Data Frame
#'
#' Sample data frame, subsets n ids
#' @param df data frame to sample
#' @param id identifier variable name as string
#' @param n number of ids to sample
#' @return a data frame
#' @export

sample_df <- function(df, id, n) {
  chosen <- sample(unique(df[, id]), n)
  df <- subset(df, eval(parse(text = id)) %in% chosen)
}

#' Random Effects Formula Combinations
#'
#' Create Vector of all Random effect formula combination for lcmem given covariates
#' of interest
#' @param cov character vector of covariates
#' @return character vector of random effects formula
#' @export
form_comb <- function(cov) {
  n <- length(cov)
  id <- unlist(lapply(1:n, function(i) utils::combn(1:n, i, simplify = F)), recursive = F)
  forms <- sapply(id, function(i) paste("~", paste(cov[i], collapse = "+")))
}
