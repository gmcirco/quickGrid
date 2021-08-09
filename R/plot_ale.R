#----------------------------------------------------#
# PLOT_ALE
#----------------------------------------------------#
#' Plot accumulated local effects
#' 
#' Uses `iml` package to plot accumulated local effects of predictor variables. Accumulated local effects (ALE) 
#' describe how a feature influences the model predictions on average. In general, ALE estimates
#' are faster and more robust alternative to partial dependence plots. For example, one might interested in 
#' how the effect of distance to a liquor store influence robberies. 
#' An ALE can determine the range of distances at which this predictor has a substantial criminogenic effect. 
#' This can also be used with density or grid count measures as well.
#'
#' @param model_list Model list output from the `gbm_fit` function
#' @param feature Predictor feature or features to be plotted. Specifying one feature will result in an ALE plot, while
#' specifying two features will create an interaction plot, showing the crossed effect of both features in a grid.
#' @param grid_size Number of points to evaluate the predictor against
#' 
#' @import iml
#' 
#' @export

plot_ale <- function(model_list, feature, grid_size = 100, max_dist = 1000){
  
  if(length(feature) > 2)
    warning("Number of features should be either 1 or 2")
  
  # Set up data
  model = model_list[[2]]
  newdata = model_list[[1]]
  
  newdata <- data.frame(newdata)
  newdata <- dplyr::select(newdata, x, y, dplyr::starts_with(c("distance.", "density.","count.")))
  
  # filter distance
  if(grepl("distance.", feature) == TRUE){
  newdata <- dplyr::filter(newdata, dplyr::across(tidyselect::matches(feature), ~ . <= max_dist))
  }
  
  rownames(newdata) <- NULL
  
  # Prediction function for iml & xgboost
  pred <- function(model, newdata)  {
    
    X <- as.matrix(newdata)
    
    results <- as.data.frame(predict(model, X))
    return(results)
  }
  
  # iml function setup
  mod <- Predictor$new(model, data = newdata, predict.function = pred)
  eff <- FeatureEffect$new(mod, feature = feature, grid.size = grid_size)
  
  # 1D ALE plot
  if(length(feature) == 1){
  plot_out <-
  ggplot(eff$results) +
    geom_line(aes_string(x = feature, y = '.value'), color = "darkblue", size = 1) +
    labs(y = "ALE",title = paste0("Accumulated Local Effect on ", feature)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12))
  }
  # 2D ALE plot
  if(length(feature) == 2){
    plot_out <-
  ggplot(eff$results, aes_string(x = feature[1], y = feature[2])) +
    geom_rect(aes(ymin = .bottom, ymax = .top, fill = .ale, xmin = .left, xmax = .right)) +
    scale_fill_viridis_c()  
    
  }
  # export plot
  plot_out
}