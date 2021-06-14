#----------------------------------------------------#
# PLOT_GBM
#----------------------------------------------------#
#' Plot output from gbm model
#'
#' Plots model output from a `gbm_fit` object. This function is primarily used to facilitate the plotting of
#' predicted probabilities on a grid-based map of a spatial region. By default, `gbm_fit` will create a basic
#' raster-based map using ggplot. Because the output is a ggplot object, is is easy to adjust or change the
#' plot manually as well. You may also provide the name of a variable you wish to plot - such as a predictor.
#' You can also plot all of your predictor variables (either distance or density) at the same time by specifying
#' `feature = 'density'` or `feature = 'distance'`.
#'
#' @param model_list Model list output from the `gbm_fit` function
#' @param feature Predictor feature to be plotted. Should be the name of a single feature to be plotted. If you
#' wish to plot all of the predictor features as a faceted plot, you may also specify `feature = 'distance'` or
#' `feature = 'density'`.
#' 
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export

plot_gbm <- function(model_list, feature = 'gbm.pred') {
  
  # Check if user wants to plot faceted density or distance plots
  
  if (feature %in% c('distance', 'density')) {
    df_plot <-
      model_list$model_dataframe %>%
      data.frame() %>%
      dplyr::select(x, y, dplyr::starts_with(feature)) %>%
      tidyr::pivot_longer(c(-x,-y), names_to = feature)
    
    plot.obj <-
      ggplot(data = df_plot) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      scale_fill_viridis_c() +
      facet_wrap( ~ .data[[feature]]) +
      coord_equal() +
      theme_void()
    
  } else{
    
    # Otherwise, perform default single plot
    # Create a basic raster plot object
    # with no other layers
    
    plot.obj <-
      ggplot(data = model_list$model_dataframe) +
      geom_raster(aes(x = x, y = y, fill = .data[[feature]])) +
      scale_fill_viridis_c() +
      coord_equal() +
      theme_void()
  }
  
  # Return final plot object
  
  plot(plot.obj)
}