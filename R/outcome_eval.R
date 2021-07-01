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
#' @param verbose
#' 
#' 
#'@export
#'
#'
#'

outcome_eval <-
  function(model_fit,
           test_data,
           eval = "pai",
           cutoff = 0.01,
           verbose = FALSE) {
    
  # Get model data from model fit
  # Needs to have the prediction column gbm.pred
  model_dataframe <- model_fit$model_dataframe
  
  # Get necessary values for PAI, PEI, and RRI
  
  
  # Run PAI
  if(eval == 'pai'){
    .pai(model_dataframe = model_dataframe,
         test_data = test_data,
         cutoff = cutoff,
         verbose = verbose)
  } else if(eval == 'rri'){
    .rri(model_dataframe = model_dataframe,
         test_data = test_data,
         cutoff = cutoff,
         verbose = verbose)
  } else if(eval == 'pei'){
    # Calculate optimal PAI
    pai_per <-
    .pei(model_dataframe = model_dataframe,
         test_data = test_data,
         cutoff = cutoff,
         verbose = FALSE)
    
    # Calculate observed PAI
    pai_obs <- 
      .pai(model_dataframe = model_dataframe,
           test_data = test_data,
           cutoff = cutoff,
           verbose = FALSE)

    # Return ratio of obs\perfect
    return(pai_obs / pai_per)
  }
  
  
}


#----------------------------------------------------#
# EVAL FUNCTIONS
#----------------------------------------------------#

# .PAI
# Calculate the predictive accuracy index (PAI)

.pai <- function(model_dataframe, test_data, cutoff, verbose = verbose){
  
  # Get the cutoff for a hotspot
  hotspot <- quantile(model_dataframe$gbm.pred, probs = 1 - cutoff)
  
  # Filter dataframe to just the hotspot regions
  # Find number of crimes within those regions
  a <- dplyr::filter(model_dataframe, gbm.pred >= hotspot)
  n <- nrow(test_data[a,])
  
  # Calculate PAI
  out <- round((n/nrow(test_data)) / cutoff,2)
  
  # Verbosity obtions
  if(verbose == TRUE){
    print(paste0("Number of areas: ", nrow(a)))
    print(paste0("Proportion of area: ", cutoff))
    print(paste0("Number of crimes: ", n))
    print(paste0("Proportion of crimes: ", round(n/nrow(test_data),2) ))
    print(paste0("PAI: ", out))
  }else{
    # Export PAI
    return(out)
  }

}

# .PEI
# Calculate predictive efficiency index
.pei <- function(model_dataframe, test_data, cutoff, verbose = verbose){
  
  # Number of areas to count
  max_a <- round(nrow(model_dataframe) * cutoff)
  
  # Join test data to model grid and obtain counts
  n_per <- sf::st_join(test_data, model_dataframe)%>%
    data.frame() %>%
    dplyr::count(grid_id) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1:max_a)
  
  # Optimal PAI for given cutoff
  out <- sum(n_per$n)/nrow(test_data) / cutoff
  
  # Export PEI
  return(out)
}

# .RRI
# Calculate recapture rate index
.rri <- function(model_dataframe, test_data, cutoff, verbose = verbose){
  
  # Get the cutoff for a hotspot
  hotspot <- quantile(model_dataframe$gbm.pred, probs = 1 - cutoff)
  
  # Filter dataframe to just the hotspot regions
  # Find number of crimes within those regions
  # and number of crimes in test period
  a <- dplyr::filter(model_dataframe, gbm.pred >= hotspot)
  n <- sum(a$gbm.pred)
  N <- nrow(test_data[a,])
  
  out <- round(n/N,2)
  
  # Verbosity obtions
  if(verbose == TRUE){
    print(paste0("Number of areas: ", nrow(a)))
    print(paste0("Proportion of area: ", cutoff))
    print(paste0("Number of crimes predicted: ", round(n,2) ))
    print(paste0("Number of crimes captured: ", N))
    print(paste0("RRI: ", out))
  }else{
    # Export PAI
    return(out)
  }
}
