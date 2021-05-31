#----------------------------------------------------#
# LGBM_FIT
#----------------------------------------------------#

# Primary fitting function
# Offers manual or cv selection of hyperparameters
# Tries to detect number of incidents per-cell
# to do either binary or poisson
# Can also override it by filling in 'model_params'

lgbm_fit <- function(prep_data,
                     model_params = NULL,
                     nleaves = 5,
                     lrate = 0.01,
                     nrounds = 1000,
                     bag_fraction = 1,
                     plot = TRUE,
                     plot_importance = TRUE,
                     cv = FALSE,
                     cv.nleaves = c(5,10,20),
                     cv.lrate = c(0.1,0.01),
                     cv.nrounds = c(250,500,750)) {
  
  # Get model dataframe 
  df <- prep_data$lgbm_dataframe
  
  # Model Outcome
  y <- df$n
  
  # Predictor Matrix
  X <- df %>%
    select(-grid_id,-n) %>%
    as.matrix()
  
  # Set up lightgbm dataset object
  dtrain <- lgb.Dataset(X, label = y)
  
  # Set up model parameters
  # First, check if only two values are 0,1
  # then assign binary
  # otherwise, check for integers
  # then assign poisson
  # otherwise, assign regression
  if(all(unique(y) %in% c(0,1)) & is.null(model_params)){
    print("Model type: binary")
    model_params <- list(objective = "binary",
                         metric = 'auc',
                         bagging_fraction = bag_fraction)
  } else if(all(y%%1 == 0) & is.null(model_params)){
    print("Model type: Poisson")
    model_params <- list(objective = "poisson",
                         metric = 'poisson',
                         bagging_fraction = bag_fraction)
  } else if(is.null(model_params) == FALSE){
    print("Using custom parameters")
  } else {
    print("Model type: regression")
    model_params <- list(objective = "regression",
                         metric = 'l2',
                         bagging_fraction = bag_fraction)
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