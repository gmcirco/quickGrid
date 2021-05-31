#----------------------------------------------------#
# PREP_DATA
#----------------------------------------------------#
#' Prepare data for analysis
#' 
#' This is a generic function that takes an outcome variable (i.e. a crime), a named list of predictor variables,
#' and a study region and converts it into a grid-based model object. Users can select whether to calculate nearest grid-cell distances,
#' densities, or grid cell counts. Function returns a (non-spatial) dataframe and a spatial (`sf`) polygon grid. The function is intended to be used with the `lightgbm` wrapper 'lgm_prep' but can be used in any other statistical model 
#' (i.e. `ranger`, `glmnet`).
#' 
#' @param outcome Outcome variable as a point shapefile
#' @param pred_var Named list of shapefiles as spatial predictors.
#' @param region Polygon shapefile enclosing the study region
#' @param gridsize Size (in feet or meters) of the size of the spatial grid.
#' @param measure Types of measures for the predictor variables (either distance, density, or both). Defaults to distance.
#' @param kernel Named list of bandwidth distances if 'density' is chosen. Defaults to automatic selection.
#' 
#' @return model list
#' @export
#' @examples
#' prep_data(outcome = crime_outcome, 
#' pred_var = spatial_predictors, 
#' region = region_shapefile, 
#' gridsize = 200)

# Prep Data for Model
prep_data <- function(outcome,
                      pred_var,
                      region,
                      gridsize,
                      measure = 'distance',
                      kernel = 'auto') {
  # Set up study grid
  area_grid <- st_make_grid(region, gridsize)
  area_grid <- area_grid[region,] %>%
    st_as_sf() %>%
    rename("geometry" = "x") %>%
    mutate(grid_id = 1:nrow(.))
  
  
  # Get xy coordinates for centroids
  # Use this as predictor below
  suppressWarnings(xy <-
                     st_coordinates(st_centroid(area_grid))[, 1:2])
  
  # Distance Measures
  # This is a change
  if (measure %in% c("distance", "both")) {
    print("Calculating distances...")
    distance_list <-
      lapply(pred_var, nearest_feature, x = area_grid)
  }
  
  # Density Measures
  # INSERT HERE
  
  # Grid Counts
  # INSERT HERE
  
  # Merge crime outcomes to the study grid
  # Count the number of outcomes per grid cell
  # then merge back to the original grid
  # finally, fill in implicit zeroes and
  # export as a dataframe
  
  suppressMessages(
    crime_outcome_grid <-
      st_join(outcome, area_grid) %>%
      count(grid_id) %>%
      data.frame() %>% 
      select(grid_id, n) %>%
      right_join(area_grid) %>%
      replace_na(list(n = 0)) %>%
      arrange(grid_id) %>%
      data.frame() %>%
      select(grid_id, n)
  )
  
  # Bind outcome data together
  # with list of distance and density values
  model_dataframe <- data.frame(xy, crime_outcome_grid,
                                lapply(distance_list, function(x) {
                                  as.numeric(unlist(x))
                                })) %>%
    setNames( c("x", "y", "grid_id", "n", names(pred_var)) )#set_names?
  return(list('lgbm_dataframe' = model_dataframe,
              'area_grid' = area_grid))
}