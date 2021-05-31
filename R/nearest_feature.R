#----------------------------------------------------#
# NEAREST FEATURE
#----------------------------------------------------#
#' Fast helper function for calculating nearest pairwise distances
#' 
#' @param x X Value. Should be grid cell shapefile
#' @param y Y Value. Should be spatial predictors

nearest_feature <- function(x,y){
  
  # Grid Centroids
  suppressWarnings(x <- st_centroid(x))
  
  # Get nearest feature
  nearest <- st_nearest_feature(x,y)
  
  # Calculate pairwise distances
  eucledian_simple <- function(x, y){
    from <- matrix(unlist(x$geometry), ncol = 2, byrow = TRUE)
    to   <- matrix(unlist(y$geometry), ncol = 2, byrow = TRUE)
    sqrt( (from[, 1] - to[, 1])^2 + (from[, 2] - to[, 2])^2 )
  }
  return(eucledian_simple(x,y[nearest,]) )
}
  