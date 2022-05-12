#' Model Class Evaluation
#'
#' Provides n and frequency of each class and post prob of classes.
#' Also plots the model results using plot_hlme function
#'
#' @param df data frame used to produce model. create_pred_df output
#' @param x hlme model, or list of hlme models. If x is a list of hlme models,
#' they should be produced from the same data (df).
#' @param age string that specifies the age or time variable used in the model.
#' If polynomial terms were included, only the linear term should be specified.
#' @param ... arguments to base plots (eg. main = "Title")
#'
#' @return kable output providing class analysis and plot output of classes
#'
#' @author William F. Mueller
#'
#' @example R\examples\display_classes.R
#'
#' @export

display_classes <- function(df, x, age, ...) {
  if (is.list(x) & class(x[[1]]) == "hlme") {
    display_classes_apply(df, x, age, ...)
  }
  else {
    sub <- paste0(
      "k = ",
      paste(model$call$ng, collapse = ""),
      "; Random = ",
      paste(model$call$random, collapse = ""),
      "; idiag = ",
      paste(model$call$idiag, collapse = ""),
      "; nwg = ",
      paste(model$call$nwg, collapse = "")
    )
    plot_hlme(
      df = df,
      model = model,
      age = age,
      sub = sub,
      ...
    )
    print(kableExtra::kable(table_classes(model), caption = sub) %>%
            kableExtra::kable_styling("striped", full_width = F))
  }
}


display_classes_apply <- function(df, model_list, age, ...) {
  for (i in 1:length(model_list)) {
    model <- model_list[[i]]
  sub <- paste0(
    "Model Number = ",
    i,
    "; k = ",
    paste(model$call$ng, collapse = ""),
    "; Random = ",
    paste(model$call$random, collapse = ""),
    "; idiag = ",
    paste(model$call$idiag, collapse = ""),
    "; nwg = ",
    paste(model$call$nwg, collapse = "")
  )
  plot_hlme(
    df = df,
    model = model,
    age = age,
    sub = sub,
    ...
  )
  print(kableExtra::kable(table_classes(model), caption = sub) %>%
          kableExtra::kable_styling("striped", full_width = F))
  }
}


#' Model Class Evaluation Table
#'
#' Creates Dataframe evaluating each class of an hlme model
#' @param model hlme model object
#' @return data frame with mean of post probability values, n, frequency.
#' @export


table_classes <- function(model) {
  n <- NULL
  mopp <- model$pprob
  mopp_sum <- mopp %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(n = dplyr::n(),
                     dplyr::across(dplyr::starts_with("prob"),
                                   ~ round(mean(.x),
                                           digits = 4))) %>%
    dplyr::mutate(freq = n / sum(n))
  return(mopp_sum)
}
