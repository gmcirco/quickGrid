#----------------------------------------------------#
# PLOT_ALE
#----------------------------------------------------#
#' Plot accumulated local effects
#' 
#' Uses `iml` package to plot accumulated local effects of predictor variables. Accumulated local effects (ALE) describe
#' how a feature influences the model predictions on average. In general, ALE estimates
#' are faster and more robust alternative to partial dependence plots. For example, one might interested in how the effect 
#' of distance to a liquor store influence robberies. An ALE can determine the range of distances at which this predictor
#' has a substantial criminogenic effect. This can also be used with density or grid count measures as well.
#'
#' 
#' @param model_list Model list output from `lgbm_fit` or `gbm_fit` function
#' @param feature Predictor feature to be plotted
#' @param grid_size Number of points to evaluate the predictor against
#' 
#' @import iml
#' 
#' @export

plot_ale <- function(model_list, feature, grid_size = 30){
  
  model = model_list[[2]]
  newdata = model_list[[1]]
  
  newdata <- data.frame(newdata)
  newdata <- dplyr::select(newdata, x, y, dplyr::starts_with(c("distance.", "density.")))
  rownames(newdata) <- NULL
  
  pred <- function(model, newdata)  {
    
    X <- as.matrix(newdata)
    
    results <- as.data.frame(predict(model, X))
    return(results)
  }
  
  mod <- Predictor$new(model, data = newdata, predict.function = pred)
  eff <- FeatureEffect$new(mod, feature = feature, grid.size = 30, method = "ale")
  
  ggplot(eff$results) +
    geom_line(aes_string(x = feature, y = '.value'), color = "darkblue", size = 1) +
    labs(y = "ALE",title = paste0("Accumulated Local Effect on ", feature)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12))
}