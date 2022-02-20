#' Loopable hlme
#'
#' This function runs the hlme function from the lcmm package, with a few tweaks to make hlme more flexible for looping purposes.
#' If ng > 1 and B is not supplied, hlme2 will run hlme with ng = 1, then use this model as B argument
#' for the specified model.
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

  # store call and initial args ------------------------------------------------
  # store call
  call <- match.call()

  # store init_args
  init_args <- names(call)

  # if ng provided and it does not equal 1, determine betas --------------------
  if ("ng" %in% init_args & "ng" != 1){
    if ("B" %in% init_args) {
      # if betas provided store betas from call -------------------------------
      if (isFALSE(silent)) cat("using betas from B arg .....\n")
      betas <- B
    } else{
      # if betas not provided calculate new beta model  ------------------------
      # create new call for beta model
      call_betas <- call

      # make function lcmm::hlme
      call_betas[[1]] <- str2lang("lcmm::hlme")

      # make group argument ng = 1 for initial betas
      if (call_betas$ng != 1) call_betas$ng <- 1

      # drop mixture because argument not allowed when ng = 1
      if ("mixture" %in% names(call_betas)) call_betas <- call_betas[names(call_betas) != "mixture"]

      # drop classmb because argument not allowed when ng = 1
      if ("classmb" %in% names(call_betas)) call_betas <- call_betas[names(call_betas) != "classmb"]

      # make nwg = FALSE
      if ("nwg" %in% names(call_betas)) call_betas$nwg <- FALSE

      # eval call betas
      if (isFALSE(silent)) cat("calculating beta model .....\n")
      betas <- eval(call_betas)
    }
  }

  # now that we have the betas we can calculate new model ----------------------

  # store new call
  new_call <- call

  # change function to lcmm::hlme
  new_call[[1]] <- str2lang("lcmm::hlme")

  # change betas
  new_call$B <- betas

  if (isFALSE(silent)) cat("calculating model .....\n")

  mo <- eval(new_call)

  mo

}
