#' Loopable hlme
#'
#' @description
#' This function runs the hlme function from the lcmm package, with a few tweaks to make hlme more flexible
#' for looping purposes. These are the tweaks:
#' * hlme2 will evaluate all arguments except B and data in the function call. This
#' way arguments can be provided as variables that are defined in an environment c
#' where hlme2 was called.
#' * If ng = 1, hlme2 will automatically drop hlme arguments that are incompatible with an ng = 1 call.
#' These arguments are mixture, classmb, B, and nwg = TRUE.
#' * If ng > 1 and B is not supplied, hlme2 will run hlme with ng = 1, and use this model as B argument
#' for the specified model.
#' * Arguments fixed, mixture, random, classmb, data, and B can be provided as character strings.
#' The strings will be coerced to formulas for fixed, mixture, random, and classmb. The strings
#' for data and B will be evaluated as symbols in the global environment.
#'
#' Note: The documentation below was inherited from the lcmm::hlme package and does not take into
#' account these alterations.
#'
#'
#' @inheritParams lcmm::hlme
#'
#' @param silent parameter to specify fi the hlme2 print statements should be silent
#'
#' @inherit lcmm::hlme return
#'
#' @inherit lcmm::hlme details
#'
#' @inherit lcmm::hlme references
#'
#' @seealso \link[lcmm]{hlme}
#'
#' @export



hlme2 <- function(fixed,
                  mixture,
                  random,
                  subject,
                  classmb,
                  ng = 1,
                  idiag = FALSE,
                  nwg = FALSE,
                  cor = NULL,
                  data,
                  B,
                  convB = 1e-04,
                  convL = 1e-04,
                  convG = 1e-04,
                  prior,
                  maxiter = 500,
                  subset = NULL,
                  na.action = 1,
                  posfix = NULL,
                  verbose = TRUE,
                  returndata = FALSE,
                  var.time = NULL,
                  partialH = FALSE,
                  silent = FALSE) {

  if (isFALSE(silent)) cat("hlme2 was called \n")
  # store call and initial args ------------------------------------------------

  # store call
  call <- match.call()
  # evaluate all arguments of the call except for [[1]], data, and B
  args <- names(call)[!(names(call) %in% c("", "data", "B"))]
  for(arg in args) {
    call[[arg]] <- eval(call[[arg]])
  }
  # coerce call if arguments are not compatible with hlme, make them compatible
  call <- helphlme::coerce_call(call)
  # store init_args
  init_args <- names(call)

  # Determine whether ng = 1 or n > 1 and proceed ------------------------------

  if (!("ng" %in% init_args) | ng == 1) {
    # if ng = 1 there is no need to calculate betas ----------------------------

    # Tell User ng = 1
    if (isFALSE(silent)) cat("ng = 1 \n")

    # drop arguments that are not compatible with ng = 1
    # store as new c
    not_compat <- c("mixture", "classmb", "nwg", "B")
    new_call <- call[!(init_args %in% not_compat)]


  } else if ("ng" %in% init_args & ng != 1) {
    # if ng >1 determine betas, determine betas and add them to new call -------

    # Tell User ng > 1
    if (isFALSE(silent)) cat("ng > 1 \n")

    if ("B" %in% init_args) {
      # if betas provided store betas from call --------------------------------

      if (isFALSE(silent)) cat("using betas from B arg .....\n")
      betas <- B

    } else{
      # if betas not provided, calculate new beta model with ng = 1 ------------

      # create new call for beta model
      call_betas <- call
      # make function lcmm::hlme
      call_betas[[1]] <- str2lang("lcmm::hlme")
      # drop arguments that are not compatible with ng = 1
      # here ng is included because if we drop ng, ng will be 1 by default
      not_compat <- c("mixture", "classmb", "nwg", "ng")
      call_betas <- call_betas[!(init_args %in% not_compat)]
      # eval call betas
      if (isFALSE(silent)) cat("calculating beta model .....\n")
      betas <- eval(call_betas)

    }
    # given betas, store new_call ----------------------------------------------

    # store new call
    new_call <- call
    # make B of new_call the symbol betas
    new_call$B <- as.symbol("betas")

  }
  # calculate mo from new_call and return mo -----------------------------------

  # change function to from hlme2 to lcmm::hlme
  new_call[[1]] <- str2lang("lcmm::hlme")
  # Calculate new model using lcmm::hlme given new_call
  if (isFALSE(silent)) cat("calculating model .....\n")
  mo <- eval(new_call)
  # tell user hlme2 call is done
  if (isFALSE(silent)) cat("hlme2 call finised \n\n")
  # return mo
  mo

}
