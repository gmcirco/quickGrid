#----------------------------------------------------#
# PREP_DATA
#----------------------------------------------------#
#' Prepare data for analysis
#' 
#' Prepares data for analysis. This is a generic function that takes an outcome variable (i.e. a crime), a named list of predictor variables,
#' and a study region and converts it into a grid-based model object. Users can select whether to calculate nearest grid-cell distances,
#' densities, or grid cell counts. Function returns a (non-spatial) dataframe and a spatial (`sf`) polygon grid. The function is intended 
#' to be used with the `lightgbm` wrapper `lgm_fit` or the `xgboost` wrapper `gbm_fit`, but can also 
#' be used in any other statistical model (i.e. `ranger`, `glmnet`).
#' 
#' @param outcome Outcome variable as a point shapefile
#' @param pred_var Named list of shapefiles as spatial predictors.
#' @param region Polygon shapefile enclosing the study region
#' @param gridsize Size (in feet or meters) of the size of the spatial grid.
#' @param measure Types of measures for the predictor variables (either distance, density, or both). Defaults to distance.
#' @param kernel Named list of bandwidth distances if 'density' is chosen. Defaults to automatic selection.
#' 
#' @return model list
#' 
#' @import sf
#' @import sp
#' @import maptools
#' @import raster
#' @import spatstat
#' @import spatstat.core
#' @import spatstat.geom
#' @import rpart
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tidyr replace_na
#' 
#' @examples
#' data("hartford_data")
#' 
#' model_data <- 
#' prep_data(outcome = hartford_data[['robbery']],
#'           pred_var = hartford_data[c("bar","liquor","gas","pharmacy","retail")],
#'           region = hartford_data[['hartford']],
#'           gridsize = 200,
#'           measure = 'distance')
#'           
#'@export

prep_data <- function(outcome,
                      pred_var,
                      region,
                      gridsize,
                      measure = 'density',
                      kernel = 'auto') {
  
  # Convert simple features to spatial
  sp_outcome <- as(outcome, "Spatial")
  
  # Designate Grid
  area_grid <- .make_grid(b = region, gridsize = gridsize)

  
  # Distance Measures
  if (measure == "distance") {
    print("Calculating distances...")
    distance_list <-
      lapply(pred_var, .nearest_feature, area_grid = area_grid)
    
    pred_values <-
      do.call(cbind.data.frame, distance_list) %>%
      setNames(paste0('distance.', names(.)))
  }
  
  # Density Measures
  if (measure == "density") {
    print("Calculating densities...")
    density_list <-
      lapply(pred_var, .kernel_density, area_grid = area_grid)
    
    pred_values <-
      do.call(cbind.data.frame, density_list) %>%
      setNames(paste0('density.', names(.)))
  }
  
  # Both
  if (measure == "both") {
    print("Calculating distance & density...")
    
    distance_list <-
      lapply(pred_var, .nearest_feature, area_grid = area_grid)
    
    dist <-
      do.call(cbind.data.frame, distance_list) %>%
      setNames(paste0('distance.', names(.)))
    
    density_list <-
      lapply(pred_var, .kernel_density, area_grid = area_grid)
    
    dens <-
      do.call(cbind.data.frame, density_list) %>%
      setNames(paste0('density.', names(.)))
    
    pred_values <- cbind.data.frame(dist,dens)
  }
  
  # Convert raster grid to sf
  area_grid_sf <- stars::st_as_stars(area_grid)
  area_grid_sf <- st_as_sf(area_grid_sf, as_points = FALSE)
  area_grid_sf$grid_id <- 1:nrow(area_grid_sf)
  area_grid_sf <- st_transform(area_grid_sf, st_crs(region))
  area_grid_sf <- area_grid_sf[region,]
  
  # Set up merged model data
  model_data <-
    cbind.data.frame(coordinates(area_grid),
                     n = .grid_count(outcome, area_grid = area_grid),
                     pred_values) %>%
    mutate(grid_id = 1:nrow(.)) %>%
    replace_na(list(n = 0)) %>%
    dplyr::filter(grid_id %in% area_grid_sf$grid_id)
  
  return(list('gbm_dataframe' = model_data,
              'area_grid' = area_grid_sf))
}


# Function to convert sf polygon into raster grid
# needed for both measures
# shamelessly borrowed from Wheeler & Steenbeck
# https://link.springer.com/article/10.1007/s10940-020-09457-7#data-availability

.make_grid <- function(b, gridsize) {
  
  # Convert simple feature to spatial
  b <- as(b, "Spatial")
  
  # Convert region to raster layer
  # Mask region for mapping
  base_raster <- raster(ext = extent(b), res = gridsize)
  projection(base_raster) <- crs(b)
  
  # Suppress annoying errors
  # This is due to garbage collection - not a real error
  # only solution at the moment
  {
    options(show.error.messages = FALSE)
    mask_raster <- rasterize(b, base_raster, getCover = TRUE) 
    options(show.error.messages = TRUE)
  }
  
  return(mask_raster)
}


# Function for rasterized grid counts
.grid_count <- function(point_data, area_grid){
  
  # Convert simple features point data to Spatial
  sp_point <- as(point_data, "Spatial")
  
  # Count up by raster cell
  # zeroes = NA values
  count_raster <- rasterize(x = sp_point, y = area_grid,fun='count')
  
  vdata <- as.data.frame(count_raster,long=TRUE)$value	
  
  return(vdata[1:length(area_grid)])
}


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


# Calculate Kernel Density
# Code also borrowed from Wheeler & Steenbeck:
# https://link.springer.com/article/10.1007/s10940-020-09457-7#data-availability

.kernel_density <- function(point_data, area_grid){
  
  # Convert simple features point data to Spatial
  sp_point <- as(point_data, "Spatial")
  
  # Set up grid window
  peval <- rasterToPoints(area_grid)[,1:2]
  spWin <- spatstat.geom::as.owin(as.data.frame(peval))
  
  # Convert Spatial object to .ppp
  # Set up grid window
  # Calculate bandwidth automatically
  # NOTE: Can change to user-selected later
  
  suppressWarnings( sp_ppp <- spatstat.geom::as.ppp(raster::coordinates(sp_point),W=spWin) )
  suppressWarnings( bdw <- bw.ppl(sp_ppp) )
  
  # Calculate Density based on chosen bandwidth
  sp_den <- spatstat.core::density.ppp(sp_ppp,sigma=bdw,edge=FALSE,warnings=FALSE) 
  
  # Now export data as a vector of density valuces
  sp_dat <- as.data.frame(sp_den)                                         
  kd_raster <- raster::rasterFromXYZ(sp_dat,res=res(area_grid),crs=crs(area_grid))
  vdata <- as.data.frame(kd_raster,long=TRUE)$value
  
  return(vdata)
  
}
