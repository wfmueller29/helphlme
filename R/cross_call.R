#' hlme argument combinations
#'
#' Provide Arguments for hlme function and this function will create a dataframe
#' where each row contains a unique hlme call derived from the unique combinations
#' of argumnets
#'
#' @param outcome character string or vector that specifies the left-hand side of the fixed
#' effects formula in the "fixed" hlme argument
#' @param fix_cov character string or vector that specifies the right-hand sige
#' of the fixed effects formula in the "fixed" hlme argument
#' @param age_var optional character string or vector that specifies the string
#' the character string to replace the substring "age_var" in the formula arguments
#' of hlme (fixed, random, mixture, classmb)
#' @param ... other named arguments values or vectors to hlme function that must
#' represented as a string, numeric, or vector.
#'
#' @return will return a dataframe where the first three columns are the values
#' for outcome, fixed_cov, and age_var (if specified), and the remaining columns contain the
#' values hlme arguments whose columns correspond to the argument names.
#'
#'

cross_call <- function(outcome, fixed_cov, age_var = NULL, ...){

  dots <- list(...)

  inputs <- c(list(outcome = outcome,
                   age_var = age_var,
                   fixed_cov = fixed_cov),
              dots)

  # null value will not work in purrr:cross_df
  if (is.null(age_var)) {
    inputs <- inputs[!(names(inputs) %in% "age_var")]
  }

  # create data with every unique call
  mo_data <- purrr::cross_df(inputs)

  # save variables which formula variables have been specified
  have_age_var <- names(mo_data)[names(mo_data) %in% c("fixed_cov", "mixture","random","classmb")]

  # if age_var specified, replace all formula with age_var
  if(!is.null(age_var)){
    mo_data[,have_age_var] <- lapply(have_age_var, function(col){
      stringr::str_replace(mo_data[[col]], pattern = "age_var", replacement = mo_data$age_var)
    })
  }

  # create fixed column
  mo_data$fixed <- paste0(mo_data$outcome, mo_data$fixed_cov)

  data.frame(mo_data)

}
