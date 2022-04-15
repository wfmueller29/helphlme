#' Coerce call to hlme
#'
#' This will coerce a call so that the call arguments are valid arguments for
#' hlme function. If the formula arguments (fixed, random, mixture, classmb)
#' is a are character strings, they will be coerce to formula. If the argument
#' data character string, it will be evaluated as a symbol in the global env.
#' The same goes for the B argument
#' @param call the call that is to be coerced
#' @return will return another call whos arguments are combatitle with the hlme
#' function call

coerce_call <- function(call){

  # find formula args provided
  formula_args <- names(call)[names(call) %in% c("fixed", "random", "mixture", "classmb")]

  # coerce formula args as formula
  for(arg in formula_args){
    if (is.character(call[[arg]])) {
      call[[arg]] <- as.formula(call[[arg]])
    }
  }

  # make data argument a dataframe
  if (is.character(call$data)) {
    call$data <- as.symbol(call$data)
  }

  # make B argument an hlme object
  if (is.character(call$B)) {
    call$B <- as.symbol(call$B)
  }

  call

}

