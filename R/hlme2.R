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

  # This function will loop through 2 cases
  # Case 1: ng = 1
  #     - Drop any argument associated with ng > 1 and call hlme
  # Case 2: ng > 1
  #     Case 2.1: Betas are not provided
  #     Case 2.1: Betas are provided

  if (isFALSE(silent)) cat("hlme2 was called \n")
  # store call and initial args ------------------------------------------------

  # store call
  call <- match.call()
  # evaluate all arguments of the call except for [[1]], data, and B
  args <- names(call)[!(names(call) %in% c("", "data", "B"))]
  for (arg in args) {
    call[[arg]] <- eval(call[[arg]])
  }
  # store init_args
  init_args <- names(call)

  # Check Case 1
  if (!("ng" %in% init_args) | ng == 1) {
    # Case 1: ng = 1 --------------------------------------------------------
    # caculate new_call by dropping ng > 1 args

    # Tell User ng = 1
    if (isFALSE(silent)) cat("ng = 1 \n")
    # drop arguments that are not compatible with ng = 1
    not_compat <- c("mixture", "classmb", "nwg", "B")
    new_call <- call[!(init_args %in% not_compat)]

    # Check Case 2
  } else if ("ng" %in% init_args & ng != 1) {
    # Case 2: ng > 1 ----------------------------------------------------------
    # ng > 1 determine new_call by keeping ng > 1 args and
    # calculating initial betas

    # Tell User ng > 1
    if (isFALSE(silent)) cat("ng > 1 \n")

    # Case 2 subcases
    if ("B" %in% init_args) {
      # Case 2.1 - store betas from call --------------------------------------
      if (isFALSE(silent)) cat("using betas from B arg .....\n")
      betas <- B
    } else {
      # Case 2.2 - calculate new betas ----------------------------------------
      # if betas not provided, calculate new beta model with ng = 1
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
    # Case 2 - calculate new_call using betas ---------------------------------
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
