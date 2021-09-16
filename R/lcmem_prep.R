#' Prep Data Frame for trajpkg functions
#'
#' This function centers and scales the variables provided by vars by subtracting by the mean
#' and dividing by the starndard deviation. It will also create new columns appending with
#' "_ns" to save the original nonscaled values.
#' @param df Dataframe object
#' @param vars character vector providing the names of the columns we would like to center and scale
#' @return a dataframe with vars centered and scaled and their nonscaled values stores as var_ns
#' @export

lcmem_prep <- function(df, vars){
  # save nonscaled values
  vars_ns <- sapply(vars, function(var) paste0(var, "_ns"))
  df[,vars_ns] <- lapply(vars, function(var){
    df[,paste0(var,"_ns")] <- df[,var]
  })
  # create summary tables
  df_sum <- lapply(vars, function(var){
    mean <- mean(df[,var])
    sd <- sd(df[,var])
    return(list(mean = mean, sd = sd))
  })
  names(df_sum) <- vars
  df_sum <- as.data.frame(df_sum)
  df_scale <- df
  # scale df
  df_scale[,vars] <-lapply(vars, function(var){
    df_scale[,var] <- (df[,var] - df_sum[,paste0(var,".mean")])/df_sum[,paste0(var,".sd")]
  })

  return(df_scale)
}
