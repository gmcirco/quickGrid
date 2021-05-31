#----------------------------------------------------#
# LGBM_FIT_CV
#----------------------------------------------------#

# Option to fit model using cross-validation
# allow user to specify a range of values for hyperparmeters
# then select the 'best' model given these parmaeters

lgbm_fit_cv <- function(dtrain, param, 
                        num_leaves,
                        learning_rate,
                        nrounds){
  
  # MODEL TUNING
  tuning_grid <- expand.grid(
    num_leaves = num_leaves,
    learning_rate = learning_rate,
    nrounds = nrounds
  )
  
  # CROSS VALIDATION TUNING
  cv.list <- list()
  for(i in 1:nrow(tuning_grid)){
    
    cv.params <- paste0("num_leaves: ", as.numeric(tuning_grid[i,][1]),
                        " learning_rate: ", as.numeric(tuning_grid[i,][2]),
                        " nrounds: ", as.numeric(tuning_grid[i,][3])) 
    print(cv.params)
    
    suppressWarnings(
      cv.out <-
        lgb.cv(data = dtrain,
               params = param,
               nrounds = as.numeric(tuning_grid[i,][3]),
               num_leaves = tuning_grid[i,][1],
               learning_rate = tuning_grid[i,][2],
               verbose = 0,
               force_col_wise=TRUE,
               nfold = 5)
    )
    
    cv.list[[i]] <- c('score' = cv.out$best_score,
                      'num_leaves' = as.numeric(tuning_grid[i,][1]),
                      'learning_rate' = as.numeric(tuning_grid[i,][2]),
                      'nrounds' = as.numeric(tuning_grid[i,][3]))
  }
  
  best_model <- Reduce(min, cv.list[1])
  
  # Fit model using optimal parameters
  gbm.fit.cv <- lightgbm(
    data = dtrain,
    params = param,
    num_leaves = best_model[[2]],
    learning_rate = best_model[[3]],
    nrounds = best_model[[4]],
  )
  
  print(best_model)
  return(gbm.fit.cv)
}