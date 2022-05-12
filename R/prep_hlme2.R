#' Prep Data Frame for trajpkg functions
#'
#' This function centers and scales the variables provided by vars by subtracting by the mean
#' and dividing by the starndard deviation. It will also create new columns appending with
#' "_ns" to save the original nonscaled values.
#' @param df Dataframe object
#' @param vars character vector providing the names of the columns we would like to center and scale
#' @param center a boolean that specifies if vars should be centered
#'   by subtracting by the mean
#' @param scale a boolean that spcifies if vars should be devided by their standard deviation
#' @param ref_data a dataframe whose values for `var` should be used to center
#' and scale the `df`
#' @return a dataframe with vars centered and scaled and their nonscaled values stores as var_ns
#' @export

prep_hlme2 <- function(df, vars, center = TRUE, scale = TRUE, ref_data = NULL) {
  # save nonscaled values
  vars_ns <- sapply(vars, function(var) paste0(var, "_ns"))
  df[, vars_ns] <- lapply(vars, function(var) {
    df[, paste0(var, "_ns")] <- df[, var]
  })
  # create summary tables
  if (is.null(ref_data)) {
    df_sum <- lapply(vars, function(var) {
      mean <- mean(df[, var])
      sd <- sd(df[, var])
      return(list(mean = mean, sd = sd))
    })
  }
  else if (is.data.frame(ref_data)) {
    df_sum <- lapply(vars, function(var) {
      mean <- mean(ref_data[, var])
      sd <- sd(ref_data[, var])
      return(list(mean = mean, sd = sd))
    })

  }
  else {
    stop("ref_data must be NULL or data.frame")
  }
  names(df_sum) <- vars
  df_sum <- as.data.frame(df_sum)
  df_scale <- df
  # scale df
  if (center) {
    df_scale[, vars] <- lapply(vars, function(var) {
      df_scale[, var] <- df[, var] - df_sum[, paste0(var, ".mean")]
    })
  }
  if (scale) {
    df_scale[, vars] <- lapply(vars, function(var) {
      df_scale[, var] <- df[, var] / df_sum[, paste0(var, ".sd")]
    })
  }


  return(df_scale)
}
