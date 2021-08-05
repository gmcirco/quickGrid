#----------------------------------------------------#
# PREP_DATA
#----------------------------------------------------#
#' Prepare data for analysis
#' 
#' Prepares data for analysis. This is a generic function that takes an outcome variable (i.e. a crime), a named list 
#' of predictor variables, and a study region and converts it into a grid-based model object. 
#' Users can select whether to calculate nearest grid-cell distances,densities, or both. Function returns a 
#' (non-spatial) dataframe and a spatial (`sf`) polygon grid. The function is intended to be used with the `xgboost` 
#' wrapper `gbm_fit`, but can also used in any other statistical model (i.e. `ranger`, `glmnet`).
#' 
#' @param outcome Outcome variable as a point shapefile
#' @param pred_var Named list of shapefiles as spatial predictors.
#' @param region Polygon shapefile enclosing the study region
#' @param gridsize Size (in feet or meters) of the size of the spatial grid.
#' @param measure Types of measures for the predictor variables (either distance, density, or both). 
#' Defaults to distance.
#' @param kernel_bdw Either a numeric value or named list of bandwidth distances if 'density' is chosen. If a named list
#' is provided, the names should match the named predictor variables in the `pred_var` object. If nothing is provided,
#' defaults to automatic bandwidth selection via spatstat::bw.ppl. If speed is a concern, manually providing bandwidth
#' values is substantially faster than the automatic selection feature.
#' @param count_vars Vector of names of variables in `pred_var` to be counted. Default is all variables.
#' 
#' @return model list
#' 
#' @import sf
#' @import sp
#' @import maptools
#' @import raster
#' @import spatstat
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' 
#' @examples
#' data("hartford_data")
#' 
#' # Prepping data for distance only
#' model_data <- prep_data(outcome = hartford_data$robbery,
#'                        pred_var = hartford_data[c('bar','nightclub','liquor','gas','pharmacy','restaurant')],
#'                        region = hartford_data$hartford,
#'                        gridsize = 200,
#'                        measure = 'distance')
#'                        
#'           
#'@export

prep_data <- function(outcome,
                      pred_var,
                      region,
                      gridsize,
                      measure = 'density',
                      kernel_bdw = 'auto',
                      count_vars) {
  
  # Temporary global suppress warnings
  # due to proj4 changes. See:
  # https://stackoverflow.com/questions/63727886/
  # proj4-to-proj6-upgrade-and-discarded-datum-warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # Check if all features are sf
  # Return error if not
  .check_features(outcome)
  .check_features(region)
  lapply(pred_var, .check_features)
  
  # Designate Grid
  area_grid <- .make_grid(b = region, gridsize = gridsize)

  # Distance Measures
  if (measure == "distance") {
    cat("Calculating distances...\n")
  
    distance_list <-
      lapply(pred_var, .nearest_feature, area_grid = area_grid)
    
    pred_values <-
      do.call(cbind.data.frame, distance_list) %>%
      setNames(paste0('distance.', names(.)))
  }
  
  # Density Measures
  if (measure == "density") {
    cat("Calculating densities...\n")
    
    # If kernel_bdw is a list, 
    # go to kernel_density list function
    if(is.list(kernel_bdw)){
      density_list <- .kernel_density_list(pred_vars = pred_var, area_grid = area_grid, bdw_opt = kernel_bdw)
    } else {
      density_list <-
        lapply(pred_var, .kernel_density, area_grid = area_grid, bdw_opt = kernel_bdw)
    }
  
    pred_values <-
      do.call(cbind.data.frame, density_list) %>%
      setNames(paste0('density.', names(.)))
  }
  
  # Both
  if (measure == "both") {
    cat("Calculating distance & density...\n")
    
    distance_list <-
      lapply(pred_var, .nearest_feature, area_grid = area_grid)
    
    dist <-
      do.call(cbind.data.frame, distance_list) %>%
      setNames(paste0('distance.', names(.)))
    
    # If kernel_bdw is a list, 
    # go to kernel_density list function
    if(is.list(kernel_bdw)){
      density_list <- .kernel_density_list(pred_vars = pred_var, area_grid = area_grid, bdw_opt = kernel_bdw)
    } else {
      density_list <-
        lapply(pred_var, .kernel_density, area_grid = area_grid, bdw_opt = kernel_bdw)
    }
    
    dens <-
      do.call(cbind.data.frame, density_list) %>%
      setNames(paste0('density.', names(.)))
    
    pred_values <- cbind.data.frame(dist,dens)
  }
  
  if(measure == "count"){
    cat("Calculating grid counts...\n")
    
    # filter count variables if provided
    # otherwise, use all pred vars
    if(is.null(count_vars) != TRUE){
      cat("Counting following variables:\n")
      cat(count_vars)
      
      pred_var <- pred_var[count_vars]
    }
    
    count_list <-
      lapply(pred_var, .grid_count, area_grid = area_grid)
    
    pred_values <-
      do.call(cbind.data.frame, count_list) %>%
      setNames(paste0('count.', names(.)))
  }
  
  
  # Convert raster grid to sf
  area_grid_sf <- stars::st_as_stars(area_grid)
  area_grid_sf <- st_as_sf(area_grid_sf, as_points = FALSE, crs)
  st_crs(area_grid_sf) <- st_crs(region)
  area_grid_sf$grid_id <- 1:nrow(area_grid_sf)
  area_grid_sf <- st_transform(area_grid_sf, st_crs(region))
  area_grid_sf <- area_grid_sf[region,]
  
  # Set up merged model data
  model_data <-
    cbind.data.frame(coordinates(area_grid),
                     n = .grid_count(outcome, area_grid = area_grid),
                     pred_values) %>%
    mutate(grid_id = 1:nrow(.)) %>%
    dplyr::filter(grid_id %in% area_grid_sf$grid_id)
  
  # Export data as list containing model data
  # and the spatial grid it was calculated on
  # required for the gbm_fit step
  return(list('gbm_dataframe' = model_data,
              'area_grid' = area_grid_sf))
  
  # Revert old warning default
  options(warn = oldw)
}


#----------------------------------------------------#
# HELPER FUNCTIONS
#----------------------------------------------------#

# .MAKE_GRID
# Function to convert sf polygon into raster grid
# needed for both measures
# shamelessly borrowed from Wheeler & Steenbeck
# https://link.springer.com/article/10.1007/s10940-020-09457-7#data-availability

.make_grid <- function(b, gridsize) {
  
  # Convert simple feature to spatial
  b <- as(b, "Spatial")
  
  # Convert region to raster layer
  base_raster <- raster(ext = extent(b), res = gridsize)
  
  # Assign projection of feature
  projection(base_raster) <- crs(b)
  
  # Mask raster to the base grid
  # Suppress annoying errors
  # This is due to garbage collection - not a real error
  # only solution at the moment
  options(show.error.messages = FALSE)
  mask_raster <- rasterize(b, base_raster, getCover = TRUE) 
  options(show.error.messages = TRUE)
  
  return(mask_raster)
}


# .GRID_COUNT
# Function for rasterized grid counts
.grid_count <- function(point_data, area_grid){
  
  # Convert simple features point data to Spatial
  sp_point <- as(point_data, "Spatial")
  
  # Count up by raster cell
  # zeroes = NA values
  count_raster <- rasterize(x = sp_point, y = area_grid,fun='count')
  
  # Export data as vector
  vdata <- as.data.frame(count_raster,long=TRUE)$value	
  vdata[is.na(vdata) == TRUE] <- 0
  
  return(vdata[1:length(area_grid)])
}

# .NEAREST_FEATURE
# Fast helper function for calculating nearest pairwise distances
# Used above for distance measures

.nearest_feature <- function(point_data, area_grid){
  
  # Convert simple features point data to Spatial
  sp_point <- as(point_data, "Spatial")
  
  # Get grid distance on raster map
  grid_dist <- distanceFromPoints(object = area_grid, xy = sp_point)
  
  # Export distance
  vdata <- as.data.frame(grid_dist,long=TRUE)$value
  
  return(vdata)
}

# .KERNEL_DENSITY
# Calculate Kernel Density
# Code also borrowed from Wheeler & Steenbeck:
# https://link.springer.com/article/10.1007/s10940-020-09457-7#data-availability

.kernel_density <- function(point_data, area_grid, bdw_opt = kernel_bdw){
  
  # Convert simple features point data to Spatial
  sp_point <- as(point_data, "Spatial")
  
  # Set up grid window
  peval <- rasterToPoints(area_grid)[,1:2]
  spWin <- spatstat.geom::as.owin(as.data.frame(peval))
  
  # Convert Spatial object to .ppp
  suppressWarnings( sp_ppp <- spatstat.geom::as.ppp(raster::coordinates(sp_point),W=spWin) )
  
  # Kernel Selection
  # If a numeric value is supplied, use the chosen value
  # Otherwise, use automatic bandwidth selection
  if (is.numeric(bdw_opt)) {
    bdw <- bdw_opt
  } else if (bdw_opt == 'auto') {
    suppressWarnings(bdw <- spatstat.core::bw.ppl(sp_ppp))
  }
  
  # Calculate Density based on chosen bandwidth
  sp_den <- spatstat.core::density.ppp(sp_ppp,sigma=bdw,edge=FALSE,warnings=FALSE) 
  
  # Now export data as a vector of density values
  sp_dat <- as.data.frame(sp_den)                                         
  kd_raster <- raster::rasterFromXYZ(sp_dat,res=res(area_grid),crs=crs(area_grid))
  vdata <- as.data.frame(kd_raster,long=TRUE)$value
  
  return(vdata)
  
}

# .KERNEL_DENSITY_LIST
# If a named list is provided for bandwidth
# Calculate kernel density separately for each value
# Using a for loop to iterate through the base .kernel_density fun

.kernel_density_list <-
  function(pred_vars, area_grid, bdw_opt = kernel_bdw) {
    
    # Check if names are provided
    if(length(names(bdw_opt)) != length(bdw_opt))
      stop("Names must be provided for all bandwidths")
    
    # Re-sort list based on the order 
    # in which predictor variables were entered
    bdw_list <-
      bdw_opt[order(factor(names(bdw_opt), levels = names(pred_vars)))]
    
    # Create empty list to hold results
    kde_list <- list()
    
    # Iterate over all predictor variables
    for (i in 1:length(pred_vars)) {
      kde_list[[i]] <-
        .kernel_density(point_data = pred_vars[[i]],
                        area_grid = area_grid,
                        bdw_opt = bdw_list[[i]])
    }
    
    # Set names on list
    names(kde_list) <- names(bdw_list)
    
    # Export list of kernel density values
    return(kde_list)
  }

# .CHECK_FEATURES
# Check is a given variable is a simple feature
# Return appropriate error if so
# (In future, automatically convert to SF?)
.check_features <- function(x){
  if(is(x, "sf") != TRUE)
    stop("All input features must be of class 'sf'")
}

