#' Create Dataframe to Predict hlme Outcome
#'
#' This funciton creates dataframes that are used by the predictY function in the lcmm package to plot hlme models
#'
#' @param df dataframe create used to model the data. Should be created by lcmem_prep fucntion
#' @param age_vars if only one age variable was used character string of that varaible name. If two or more age
#' variables were used, character vector where each element is the name of the age varaible. The first element
#' be the first order age term (ie. c("age_wk", "age_wk^2"))
#' @param fixcov default is null. Named vector of nontime-dependent covariates included in the the model. The names
#' of the entries corresponds to the name of the variable and the value of the entries corresponds to group
#' that we would like to predict.
#' @return a dataframe with age varaibles scaled and not scaled ("_ns"), and time-independent covariates to use
#' for lcmm::predY function
#' @export

create_pred_df <- function(df, age_vars, fixcov = NULL) {
  if (!is.null(fixcov)) { ## if fixcov are provided
    fixcov_names <- names(fixcov) ## get names of fixcov
    df[, fixcov_names] <- lapply(fixcov_names, function(name) { ## loop through names
      val <- fixcov[[name]] ## assigned fixcov value provided
    })
  } else {
    fixcov_names <- NULL
  }

  age_vars_ns <- paste0(age_vars, "_ns") # create non-scaled varaible names
  df <- df[, c(age_vars, age_vars_ns, fixcov_names)] # select age vars scaled and age vars not scaled
  df <- df[!duplicated(df[, age_vars]), ] # removed duplicated times and select first occurring
  df$cut_age <- cut(df[, age_vars[1]], breaks = 50) # cut range into 50 intervals based on age_wk
  df <- df[order(df[, age_vars[1]]), ] # order dataframe based on age_wk
  df_not_dup <- df[!duplicated(df$cut_age), ] # select first occuring observation for each cut_age group
  df <- df[, names(df) != "cut_age"] # drop cut_age variable
  return(df)
}
