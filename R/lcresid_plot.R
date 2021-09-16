#' Plot the residuals of hlme model by class
#'
#' This function will plot the residuals of an hlme model by class
#' @param data data frame that was used to create the model
#' @param model hlme model output that we would like to plot
#' @param nameofoutcome string name of outcome variable
#' @param nameofage string name of age variable
#' @param ylimit numeric 2 element vector specifying y minimum and y maximum
#' @return ggplot object of plotted residuals by class
#' @export

### residual plot by class -----------------------------------------------------
lcresid_plot <- function(data, model, nameofoutcome,  nameofage, ylimit=c(-5,5)){
  k     <- model$ng
  preds <- model$pred
  names(preds)[6] <- nameofoutcome
  nameofid        <- names(model$pred)[1]
  test <- dplyr::left_join(preds, model$pprob, by=nameofid)
  test <- dplyr::left_join(test, data, by=c(nameofid, nameofoutcome))

  plotvalues <- NULL

  plots <- list()

  for(i in 1:k){

    newplotvalues <- test %>% dplyr::filter(class==i) %>% dplyr::mutate(Residuals=get(nameofoutcome)-eval(parse(text=paste0("pred_ss",i))))
    plotvalues <- rbind(plotvalues, newplotvalues)

    plotvaluessub <- plotvalues %>% dplyr::filter(class==i)



    plots[[i]] <- ggplot2::ggplot(data = plotvaluessub, aes(x = eval(as.symbol(nameofage)), y = Residuals, group = class))+
      theme(axis.text=element_text(size=16),text = element_text(size=16)) +
      geom_point() +
      #stat_summary(fun=mean, geom="line", size = 3, col="CadetBlue", group=1) +
      geom_smooth()+
      ggtitle("Residuals in class", i) +
      coord_cartesian(ylim = ylimit)

  }
  a <- ggpubr::ggarrange(plotlist =  plots, ncol = 2)
  return(a)
}

