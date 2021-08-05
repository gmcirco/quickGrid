#----------------------------------------------------#
# GBM_FIT
#----------------------------------------------------#
#' Fit a gradient boosted tree model
#' 
#' This is essentially a wrapper around the existing `xgboost` function. This function provides a number of conveniences to speed up model building,
#' including some help selecting reasonable parameters, evaluating fit, and examining variable importance. 
#' Users should provide data prepared using the 'prep_data' function. 
#' By default, `gbm_fit` attempts to fit a reasonable model using built-in parameters, or using cross-validation (preferred!). 
#' Users are STRONGLY encouraged to tune the model parameters using some estimate of out-of-sample prediction. 
#' The built-in cross-validation step greatly aids this step by testing a range of values in a tuning grid, 
#' finding the model that minimizes the chosen loss function, then fitting using the optimal set of parameters.
#' 
#' @param prep_data Model list output from 'prep_data' function.
#' @param model_params Optional `xgboost` model parameters. Defaults to NULL.
#' @param eta Step size shrinkage (aka 'learning rate). Lower values imply smaller steps and higher shrinkage.
#' @param gamma Minimum loss required to make a partition on a leaf node. Larger values imply more conservative models
#' @param max_depth Maximum tree depth. Higher values imply more complex trees, but also more overfitting.
#' @param min_child_weight Minimum weight required for a leaf node. Higher values imply more conservative trees.
#' @param subsample Ratio of observations to randomly sub sample each boosting iteration. Lower values tend to help prevent overfitting.
#' @param plot_importance Should variable importance values be plotted? Defaults to TRUE.
#' @param cv Should model parameters be chosen using cross validation? Defaults to FALSE.
#' @param cv.eta Step size shrinkage (aka 'learning rate). Lower values imply smaller steps and higher shrinkage.
#' @param cv.gamma Minimum loss required to make a partition on a leaf node. Larger values imply more conservative models
#' @param cv.max_depth Maximum tree depth. Higher values imply more complex trees, but also more overfitting.
#' @param cv.min_child_weight Minimum weight required for a leaf node. Higher values imply more conservative trees.
#' @param cv.subsample Ratio of observations to randomly sub sample each boosting iteration. Lower values tend to help prevent overfitting.
#' @param cv.nrounds Maximum number of boosting rounds. More rounds are needed for lower values of eta.
#' 
#' @import xgboost
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf
#' @importFrom dplyr relocate
#' @importFrom forcats fct_reorder
#' 
#' @examples 
#' 
#' data("hartford_data")
#' 
#' # Prepping data for distance only
#' model_data <- prep_data(outcome = hartford_data$robbery,
#'                        pred_var = hartford_data[c('bar','nightclub','liquor','gas','pharmacy','restaurant')],
#'                        region = hartford_data$hartford,
#'                        gridsize = 200,
#'                        measure = 'distance')
#'           
#' fit1 <- gbm_fit(
#'   prep_data = model_data,
#'   eta = 0.3,
#'   gamma = 1,
#'   max_depth = 6,
#'   min_child_weight = 1,
#'   subsample = .5,
#'   nrounds = 1000,
#'   plot_importance = TRUE
#'   )
#' 
#' @export

#---------------------------#
# GBM_FIT
#---------------------------#
# Primary fitting function
#   Takes either manual input, or fits parameters using cross-validation

gbm_fit <- function(prep_data,
                    model_params = NULL,
                    eta = 0.3,
                    gamma = 2,
                    max_depth = 6,
                    min_child_weight = 1,
                    subsample = .5,
                    nrounds = 1000,
                    plot_importance = TRUE,
                    cv = FALSE,
                    cv.random = FALSE,
                    cv.random.iters = 3,
                    cv.folds = 5,
                    cv.eta = c(0.3,0.1),
                    cv.gamma = c(1),
                    cv.maxdepth = c(6,12,20),
                    cv.min_child_weight = c(1),
                    cv.subsample = c(.75,.5),
                    cv.nrounds = c(500,1000)) {
  
  # Get model dataframe 
  
  df <- prep_data$gbm_dataframe
  
  # Model Outcome
  
  y <- df$n
  
  # Predictor Matrix
  
  X <- df
  X <- df[!names(df) %in% c("grid_id","n")]
  X <- as.matrix(X)
  
  # Set up lightgbm dataset object
  
  xtrain <- xgb.DMatrix(label = y, data = X)
  
  # Set up model parameters
  # First, check if only two values are 0,1
  # then assign binary
  # otherwise, check for integers
  # then assign poisson
  # otherwise, assign regression
  
  if(is.null(model_params) == FALSE){
    cat("Using custom parameters\n")
  } else if(all(unique(y) %in% c(0,1)) & is.null(model_params)){
    
    cat("Model type: binary\n")
    objective <- "binary:logistic"
    eval_metric <- "logloss"
    
  } else if(all(y%%1 == 0) & is.null(model_params)){
    cat("Model type: Poisson\n")
    objective <- "count:poisson"
    eval_metric <- 'poisson-nloglik'
    
  } else {
    cat("Model type: regression\n")
    objective <- "reg:squarederror"
    eval_metric <- "rmse"
  }
  
  # Set up parameters
  param <-
    list(
      eta = eta,
      gamma = gamma,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      objective = objective,
      eval_metric = eval_metric)
  
  # Model Fitting
  # Either fit using cross validation
  # or default\user-specified options
  
  if(cv == TRUE){
    cat("Fitting XGBoost model via cross validation...\n")
    
    if(cv.random == TRUE)
    cat("Using random grid search\n")

    gbm.fit <- .gbm_fit_cv(xtrain = xtrain, 
                           folds = cv.folds,
                           cv.random = cv.random,
                           cv.random.iters = cv.random.iters,
                           eta = cv.eta,
                           gamma = cv.gamma,
                           maxdepth = cv.maxdepth,
                           min_child_weight = cv.min_child_weight,
                           subsample = cv.subsample,
                           nround = cv.nrounds,
                           objective = objective,
                           eval_metric = eval_metric)
    
  }
  else{
    cat("Fitting gbm model...\n")
    
    gbm.fit <- 
      xgb.train(
        param,
        xtrain,
        nrounds = nrounds)
    
  }
  
  # Get predictions
  
  gbm.pred <- predict(gbm.fit, X)
  
  # Get feature importance
  
  gbm.imp <- xgb.importance(feature_names = gbm.fit$feature_names, model = gbm.fit)
  
  # Add Predictions to Study Grid
  
  gbm.fit.pred <- prep_data$area_grid %>%
    dplyr::select(-grid_id) %>%
    cbind.data.frame(df, gbm.pred) %>%
    st_as_sf() %>%
    relocate(grid_id)
  
  # Plot importance
  
  if (plot_importance == TRUE)
    .plot_importance(gbm.imp)
  
  
  # Return named list
  # Model dataframe as a shapefile with predictions
  # Original model used to fit predictions
  
  return(list('model_dataframe' = gbm.fit.pred,
              'model_fit' = gbm.fit) )
  
}


#---------------------------#
# PLOT IMPORTANCE
#---------------------------#
# Variable importance plot

.plot_importance <- function(x){
  
  # Set default fontsize
  
  fontsize = 12
  
  # Adjust Size for lots of vars
  
  if(nrow(x) >= 10)
    fontsize <- 9
  
  # Plot out results as bar chart
  plot(
    ggplot2::ggplot(x) +
      ggplot2::geom_col(ggplot2::aes(x = Gain, 
                                     y = fct_reorder(Feature, Gain)), 
                        fill = "Darkblue") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = element_text(size = fontsize),
        axis.title.y = element_blank()
      )
  )
}

#---------------------------#
# GBM_FIT_CV
#---------------------------#
# Optional cross-validation for parameters

.gbm_fit_cv <- function(xtrain, 
                        folds,
                        cv.random,
                        cv.random.iters,
                        eta,
                        gamma,
                        maxdepth,
                        min_child_weight,
                        subsample,
                        nrounds,
                        objective,
                        eval_metric){
  
  # Set up tuning grid
  tuning_grid <- expand.grid(
    eta = eta,
    gamma = gamma,
    maxdepth = maxdepth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    nrounds = nrounds
  )
  
  # if random search selected, select subset of grid
  if(cv.random == TRUE){
  cat(paste0("\n","Grid size: ", nrow(tuning_grid),"\n"))
  cat(paste0("Proportion of grid sampled: ", round(cv.random.iters/nrow(tuning_grid),3),"\n" ))
  tuning_grid <- tuning_grid[sample(nrow(tuning_grid), cv.random.iters),]
  }
  
  
  # Set up tuning grid
  
  cv.list <- list()
  for(i in 1:nrow(tuning_grid)){
    
    param <-
      list(
        eta = as.numeric(tuning_grid[i,][1]),
        gamma = as.numeric(tuning_grid[i,][2]),
        max_depth = as.numeric(tuning_grid[i,][3]),
        min_child_weight = as.numeric(tuning_grid[i,][4]),
        subsample = as.numeric(tuning_grid[i,][5]),
        objective = objective,
        eval_metric = eval_metric)
    
    # Print status of cv
    print(unlist(param))
    
    # Fit cross-validated model
    # Using parameters from tuning grid
    # Set early stopping to 10% of total iterations
    # (possibly allow changing in the future?)
    cv.out <-
      xgb.cv(
        data = xtrain,
        params = param,
        nrounds = as.numeric(tuning_grid[i,][6]),
        early_stopping_rounds = .1*as.numeric(tuning_grid[i,][6]),
        verbose = FALSE,
        nfold = folds)
    
    # Save results in a list
    # Use to select best model, based on chosen score
    # (possibly allow changing loss function later?)
    cv.list[[i]] <- c('score' = min(cv.out$evaluation_log$test_poisson_nloglik_mean),
                      'eta' = as.numeric(tuning_grid[i,][1]),
                      'gamma' = as.numeric(tuning_grid[i,][2]),
                      'max_depth' = as.numeric(tuning_grid[i,][3]),
                      'min_child_weight' = as.numeric(tuning_grid[i,][4]),
                      'subsample' = as.numeric(tuning_grid[i,][5]),
                      'nrounds' = as.numeric(tuning_grid[i,][6]))
  }
  
  # Select best model from cv
  # either binary or poisson log loss
  # or mse for regression
  best_model <- as.data.frame(do.call(rbind, lapply(cv.list, unlist)))
  best_model <- best_model[which.min(best_model$score),]
  
  # Fit model using optimal parameters
  best_param <-
    list(
      eta = as.numeric(best_model['eta']),
      gamma = as.numeric(best_model['gamma']),
      max_depth = as.numeric(best_model['max_depth']),
      min_child_weight = as.numeric(best_model['min_child_weight']),
      subsample = as.numeric(best_model['subsample']),
      objective = objective,
      eval_metric = eval_metric)
  
  gbm.fit.cv <- 
    xgb.train(
      best_param,
      xtrain,
      nrounds = as.numeric(best_model['nrounds']) )
  
  cat("Best model:","\n")
  print(best_model)
  return(gbm.fit.cv)
}