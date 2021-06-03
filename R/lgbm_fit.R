#----------------------------------------------------#
# LGBM_FIT
#----------------------------------------------------#
#' Fit a `lightgbm` model
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
#' @import lightgbm
#' @import ggplot2
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
#' @importFrom forcats fct_reorder
#' 
#' @export

lgbm_fit <- function(prep_data,
                     model_params = NULL,
                     nleaves = 5,
                     lrate = 0.1,
                     nrounds = 500,
                     bag_frac = 1,
                     bag_freq = 0,
                     plot = TRUE,
                     plot_importance = FALSE,
                     cv = FALSE,
                     cv.folds = 5,
                     cv.nleaves = c(3,14,30),
                     cv.mindata = c(50,100,200),
                     cv.maxdepth = c(7,15,25),
                     cv.lrate = c(0.01),
                     cv.nrounds = c(750)) {
  
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
                         metric = 'binary',
                         bagging_fraction = bag_frac,
                         bagging_freq = bag_freq,
                         verbose = -1)
  } else if(all(y%%1 == 0) & is.null(model_params)){
    print("Model type: Poisson")
    model_params <- list(objective = "poisson",
                         metric = 'poisson',
                         bagging_fraction = bag_frac,
                         bagging_freq = bag_freq,
                         verbose = -1)
  } else {
    print("Model type: regression")
    model_params <- list(objective = "regression",
                         metric = 'l2',
                         bagging_fraction = bag_frac,
                         bagging_freq = bag_freq,
                         verbose = -1)
  }
  
  
  # Model Fitting
  # Either fit using cross validation
  # or default\user-specified options
  if(cv == TRUE){
    print("Fitting lgbm model via cross validation...")
    gbm.fit <- .lgbm_fit_cv(dtrain = dtrain,
                           param = model_params,
                           folds = cv.folds,
                           num_leaves = cv.nleaves,
                           min_data_in_leaf = cv.mindata,
                           max_depth = cv.maxdepth,
                           learning_rate = cv.lrate,
                           nrounds = cv.nrounds)
    
  }
  else{
    print("Fitting lgbm model...")
    suppressWarnings(
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
        verbose = -1
      )
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
# Variable importance plot
.plot_importance <- function(x){
  
  # Set default fontsize
  
  fontsize = 12
  
  # Adjust Size for lots of vars
  
  if(nrow(x) >= 10)
    fontsize <- 9
  
  # Plot out results as bar chart
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
# Map of predictions
.plot_map <- function(x){
  
  # Plot out predictions
  plot(
    ggplot() +
      geom_sf(data = x, aes(fill = gbm.pred), color = NA) +
      labs(fill = "Prediction") +
      scale_fill_viridis_c() +
      scale_color_viridis_c() +
      theme_void()
  )
}

# LGBM_FIT_CV
# Optional cross-validation for hyperparameters
.lgbm_fit_cv <- function(dtrain, 
                        param,
                        folds,
                        num_leaves,
                        min_data_in_leaf,
                        max_depth,
                        learning_rate,
                        nrounds){
  
  # Set up tuning grid
  
  tuning_grid <- expand.grid(
    num_leaves = num_leaves,
    min_data_in_leaf = min_data_in_leaf,
    max_depth = max_depth,
    learning_rate = learning_rate,
    nrounds = nrounds
  )
  
  # CROSS VALIDATION TUNING
  
  cv.list <- list()
  for(i in 1:nrow(tuning_grid)){
    
    cv.params <- paste0("num_leaves: ", as.numeric(tuning_grid[i,][1]),
                        " min_data_in_leaf: ", as.numeric(tuning_grid[i,][2]),
                        " max_depth: ", as.numeric(tuning_grid[i,][3]),
                        " learning_rate: ", as.numeric(tuning_grid[i,][4]),
                        " nrounds: ", as.numeric(tuning_grid[i,][5])) 
    print(cv.params)
    
    suppressWarnings(
      suppressMessages(
      cv.out <-
        lgb.cv(data = dtrain,
               params = param,
               num_leaves = tuning_grid[i,][1],
               min_data_in_leaf = tuning_grid[i,][2],
               max_depth = tuning_grid[i,][3],
               learning_rate = tuning_grid[i,][4],
               nrounds = as.numeric(tuning_grid[i,][5]),
               verbose = 0,
               force_col_wise=TRUE,
               nfold = folds)
    )
    )
    
    cv.list[[i]] <- c('score' = cv.out$best_score,
                      'num_leaves' = as.numeric(tuning_grid[i,][1]),
                      'min_data_in_leaf' = as.numeric(tuning_grid[i,][2]),
                      'max_depth' = as.numeric(tuning_grid[i,][3]),
                      'learning_rate' = as.numeric(tuning_grid[i,][4]),
                      'nrounds' = as.numeric(tuning_grid[i,][5]))
  }
  
  # Select best model from cv
  # either binary or poisson log loss
  # NOTE: If regression, need to MAXIMIZE l2
  
  best_model <- as.data.frame(do.call(rbind, lapply(cv.list, unlist)))
  best_model <- best_model[which.min(best_model$score),]
  
  # Fit model using optimal parameters
  
  gbm.fit.cv <- lightgbm(
    data = dtrain,
    params = param,
    num_leaves = best_model[[2]],
    min_data_in_leaf = best_model[[3]],
    max_depth = best_model[[4]],
    learning_rate = best_model[[5]],
    nrounds = best_model[[6]],
  )
  
  print(best_model)
  return(gbm.fit.cv)
}