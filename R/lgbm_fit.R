#----------------------------------------------------#
# LGBM_FIT
#----------------------------------------------------#
#' Fit `lightgbm` model
#' 
#' This is a wrapper around the `lightgbm` function. This function provides a number of conveniences to speed up model building. 
#' Users should provide data prepared using the 'prep_data' function. 'lgmb_fit' attempts to fit a reasonable model using some default
#' parameters, or using cross-validation (preferred!). Users are STRONGLY encouraged to tune the model hyperparameters using some
#' estimate of out-of-sample prediction. The built-in cross-validation step greatly aids this step by testing a range of values in a tuning grid, 
#' finding the best model, then fitting using the chosen parameters.
#' 
#' @param prep_data Model list output from 'prep_data' function.
#' @param model_params Optional `lightgbm` model parameters. Defaults to NULL.
#' @param nleaves Maximum number of leaves in one tree. Defaults to 5.
#' @param lrate Learning rate. Controls the deepness or shallowness of each iteration. Smaller steps generally 
#' produce lower test error, but require a larger number of iterations. Defaults to 0.01.
#' @param bag_frac Proportion of sample to randomly subset. Defaults to 1.
#' @param bag_freq How often to perform bagging. Defaults to 0.
#' @param plot Plot a prediction map. Defaults to TRUE.
#' @param plot_importance Plot variable importance values. Defaults to FALSE.
#' @param cv Should hyperparameters be chosen using cross validation? Defaults to FALSE.
#' @param cv.nleaves Vector of potential values for number of leaves in tuning grid.
#' @param cv.lrate Vector of potential values for learning rate in tuning grid.
#' @param cv.nrounds Vector of potential values for number of rounds in tuning grid.
#' 
#' @importFrom lightgbm lightgbm
#' @importFrom lightgbm lgb.importance
#' @importFrom lightgbm lgb.Dataset
#' @importFrom magrittr %>%
#' @importFrom sf st_make_grid
#' @importFrom sf st_as_sf
#' @importFrom sf st_centroid
#' @importFrom sf st_coordinates
#' @importFrom sf st_nearest_feature
#' @importFrom sf st_join
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom dplyr right_join
#' @importFrom dplyr relocate
#' @importFrom tidyr replace_na
#' @importFrom forcats fct_reorder
#' @import ggplot2


lgbm_fit <- function(prep_data,
                     model_params = NULL,
                     nleaves = 5,
                     lrate = 0.01,
                     nrounds = 1000,
                     bag_frac = 1,
                     bag_freq = 0,
                     plot = TRUE,
                     plot_importance = FALSE,
                     cv = FALSE,
                     cv.nleaves = c(5,10,20),
                     cv.lrate = c(0.1,0.01),
                     cv.nrounds = c(250,500,750)) {
  
  # Get model dataframe 
  df <- prep_data$lgbm_dataframe
  
  # Model Outcome
  y <- df$n
  
  # Predictor Matrix
  X <- df
  X <- df[!names(df) %in% c("grid_id","n")]
  X <- as.matrix(X)
  
  # Set up lightgbm dataset object
  dtrain <- lgb.Dataset(X, label = y)
  
  # Set up model parameters
  # First, check if only two values are 0,1
  # then assign binary
  # otherwise, check for integers
  # then assign poisson
  # otherwise, assign regression
  
  if(is.null(model_params) == FALSE){
    print("Using custom parameters")
  } else if(all(unique(y) %in% c(0,1)) & is.null(model_params)){
    print("Model type: binary")
    model_params <- list(objective = "binary",
                         metric = 'auc',
                         bagging_fraction = bag_frac,
                         bagging_freq = bag_freq)
  } else if(all(y%%1 == 0) & is.null(model_params)){
    print("Model type: Poisson")
    model_params <- list(objective = "poisson",
                         metric = 'poisson',
                         bagging_fraction = bag_frac,
                         bagging_freq = bag_freq)
  } else {
    print("Model type: regression")
    model_params <- list(objective = "regression",
                         metric = 'l2',
                         bagging_fraction = bag_frac,
                         bagging_freq = bag_freq)
  }
  
  
  # Model Fitting
  # Either fit using cross validation
  # or default\user-specified options
  if(cv == TRUE){
    print("Fitting lgbm model via cross validation...")
    gbm.fit <- lgbm_fit_cv(dtrain = dtrain,
                           param = model_params,
                           num_leaves = cv.nleaves,
                           learning_rate = cv.lrate,
                           nrounds = cv.nrounds)
  }
  else{
    print("Fitting lgbm model...")
    suppressMessages(
      gbm.fit <- lightgbm(
        data = dtrain,
        params = model_params,
        num_leaves = nleaves,
        learning_rate = lrate,
        nrounds = nrounds,
        bagging_fraction = bag_frac,
        bagging_freq = bag_freq,
        force_col_wise=TRUE,
        verbose = 0
      )
    )
  }
  # Get predictions
  gbm.pred <- predict(gbm.fit, X)
  
  # Get feature importance
  gbm.imp <- lgb.importance(gbm.fit)
  
  # Add Predictions to Study Grid
  gbm.fit.pred <- prep_data$area_grid %>%
    select(-grid_id) %>%
    cbind.data.frame(df, gbm.pred) %>%
    st_as_sf() %>%
    relocate(grid_id)
  
  # Plot gbm
  if (plot == TRUE)
    .plot_map(gbm.fit.pred)
  
  
  # Plot importance
  if (plot_importance == TRUE)
    .plot_importance(gbm.imp)
  
  
  # Return named list
  # Model dataframe as a shapefile with predictions
  # Original model used to fit predictions
  return(list('orginal_data' = df,
              'model_dataframe' = gbm.fit.pred,
              'model_fit' = gbm.fit) )
  
}


# PLOT IMPORTANCE
# Plot importance of variables
.plot_importance <- function(x){
  
  # Set default fontsize
  fontsize = 12
  
  # Adjust Size for lots of vars
  if(nrow(x) >= 10)
    fontsize <- 9
  
  plot(
    ggplot(x) +
      geom_col(aes(x = Gain, 
                   y = fct_reorder(Feature, Gain)), 
               fill = "Darkblue") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = fontsize),
        axis.title.y = element_blank()
      )
  )
}

# PLOT MAP
# Plot predictions onto a map
.plot_map <- function(x){
  plot(
    ggplot() +
      geom_sf(data = x, aes(fill = gbm.pred), color = NA) +
      labs(fill = "Prediction") +
      scale_fill_viridis_c() +
      scale_color_viridis_c() +
      theme_void()
  )
}