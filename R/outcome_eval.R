#----------------------------------------------------#
# OUTCOME_EVAL
#----------------------------------------------------#
#' Evaluate predictions from a gbm_fit model
#'
#' Evaluate predictions from a fitted gbm_fit model. Provides a variety of functions to estimate the out-of-sample
#' effectiveness of a prediction model, including the predictive accuracy index (PAI), the predictive efficiency index
#' (PEI), and the recapture rate index (RRI). At a minimum users must provide a fitted model object from the
#' `gbm_fit` function and a set of out-of-sample observations as 'test' data. These out-of-sample values should be
#' crimes or observations that were not used in the fitting the model.
#'
#' @param model_fit
#' @param test_data
#' @param eval
#' @param cutoff
#'
#'
#'@export
#'

outcome_eval <-
  function(model_fit,
           test_data,
           eval = "pai",
           cutoff = 0.01) {
    # Get model data from model fit
    # Needs to have the prediction column gbm.pred
    model_dataframe <- model_fit$model_dataframe
    
    # Get necessary values for PAI, PEI, and RRI
    # Get the cutoff for a hotspot
    hotspot <-
      quantile(model_dataframe$gbm.pred, probs = 1 - cutoff)
    
    # Global Variables for sub-functions
    # a = number of hotspot regions
    # n = number of crimes in hotspot regions
    # n_sum = total number of predicted crimes in hotspots
    # max_a = number of areas in hotspot regions
    a <- dplyr::filter(model_dataframe, gbm.pred >= hotspot)
    n <- nrow(test_data[a,])
    n_sum <- sum(a$gbm.pred)
    max_a <- round(nrow(model_dataframe) * cutoff)
    
    
    # Initalize list to hold results
    eval_list <- list()
    
    # Run PAI
    if (any(eval %in% 'pai'))
      eval_list[[1]] <- .pai(a, n, test_data, cutoff)
    
    # Run PEI
    if (any(eval %in% 'pei'))
      eval_list[[2]] <- .pei(a, max_a, n, test_data, model_dataframe, cutoff)
    
    # Run RRI
    if (any(eval %in% 'rri'))
      eval_list[[3]] <- .rri(a, n, n_sum, test_data, cutoff)
    
    # Filter output
    eval_list <- unlist(eval_list[lengths(eval_list) != 0])
    
    return(eval_list)
  }

#----------------------------------------------------#
# EVAL FUNCTIONS
#----------------------------------------------------#

# .PAI
# Calculate the predictive accuracy index (PAI)

.pai <-
  function(a = a,
           n = n,
           test_data = test_data,
           cutoff = cutoff) {
    
    # Calculate PAI
    out <- c("PAI" = round((n / nrow(test_data)) / cutoff, 2))
    
    # Export PAI
    return(out)
  }

# .PEI
# Calculate predictive efficiency index
.pei <-
  function(a = a,
           max_a = max_a,
           n = n,
           test_data = test_data,
           model_dataframe = model_dataframe,
           cutoff = cutoff) {
    
    # Join test data to model grid and obtain counts
    n_per <- sf::st_join(test_data, model_dataframe) %>%
      data.frame() %>%
      dplyr::count(grid_id) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1:max_a)
    
    # Optimal PAI for given cutoff
    pai_per <- sum(n_per$n) / nrow(test_data) / cutoff
    pai_obs <- as.numeric(.pai(a, n, test_data, cutoff))
    
    # Calculate ratio of observed to perfect
    out <- c("PEI" = round(pai_obs / pai_per, 2))
    
    # Export PEI
    return(out)
  }

# .RRI
# Calculate recapture rate index
.rri <-
  function(a = a,
           n = n,
           n_sum = n_sum,
           test_data = test_data,
           cutoff = cutoff) {
    
    # Calculate RRI
    out <- c("RRI" = round(n_sum / n, 2))
    
    # Export PAI
    return(out)
  }
