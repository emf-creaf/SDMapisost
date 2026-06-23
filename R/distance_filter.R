#' @title
#' Distance-based spatial filtering
#' 
#' @description
#' Function \code{distance_filter} applies a spatial filter to a set of N-dimensional coordinates such that
#' the resulting points are separated by at least a minimum distance.
#' 
#' @param df numeric \code{data.frame} with as many rows as points to be included in the analysis.
#' @param min_dist \code{numeric}, minimum distance value between points.
#' @param strictly \code{logical}, set it to \code{TRUE} to evaluate distance strictly larger than \code{min_dist}.
#' Default is \code{FALSE}, i.e. distances can be larger or equal to \code{min_dist}.
#' @param columns character vector, name of the columns in \code{df} to be used to measure distance.
#' @param method The method to be used (see \link[stats]{dist} for details).
#' @param shuffle  \code{logical}, set it to \code{TRUE} to randomly shuffle the points in \code{df}.
#' If not set, algorithm will always start at first row and will move down to the last one.
#' By default it is set to \code{TRUE}, which means that every time \code{distance_filter} is
#' called the result will likely be different.
#' @param verbose logical, if TRUE a progress bar will be printed on terminal.
#'
#' @return
#' A \code{data.frame} with as many columns as \code{df} and with rows containing points that are
#' separated by at least a distance \code{min_dist}. Coordinates for points can be of any dimension, i.e.
#' \code{df} can have as many columns as needed (distances are calculated with the built-in \link[stats]{dist} function).
#' If all distances are less than \code{min_dist}, NA is returned.
#' 
#' @details
#' Simple algorithm that goes point by point checking its distance to all others.
#' 
#' @export
#'
#' @examples
#' # install.packages(c("sf", "gstat", "sp))
#' 
#' library(sp)
#' data(meuse)
#' 
#' # First we plot a variogram with all points.
#' meuse_sf <- sf::st_as_sf(meuse, coords = c("x", "y"), crs = NA)
#' cutoff <- 4000
#' vmeuse <- gstat::variogram(log(zinc) ~ 1, data = meuse_sf, boundaries = boundaries, cutoff = cutoff)
#' plot(vmeuse, main = "Variogram", type="l", lwd=2,xlim=c(0, 4000), ylim=c(0, 1.5))
#' 
#' # We now choose distance = 500 as our minimum distance value.
#' min_dist <- 500
#' cutoff <- 4000
#' boundaries <- seq(min_dist, cutoff, by = 250)
#' 
#' # Compute and plot variogram. Every time 'distance_filter' is run the resulting filtered 
#' # dataset is different. 
#' meuse_filtered <- distance_filter(meuse, min_dist = min_dist, columns = c("x", "y"), verbose = FALSE)
#' meuse_filtered_sf <- sf::st_as_sf(meuse_filtered, coords = c("x", "y"), crs = NA)
#' v1 <- gstat::variogram(log(zinc) ~ 1, data = meuse_filtered_sf, boundaries = boundaries, cutoff = cutoff)
#' plot(v1, main = "Filtered variogram", xlim = c(0, 4000), ylim = c(0, 1.5))
#' 
distance_filter <- function(df, min_dist, strictly = TRUE, columns = NULL, method = "euclidean", shuffle = T, verbose = T) {


  # Input must be a data.frame or a 2D matrix.
  stopifnot("Input 'df' must be a data.frame or a matrix" = any(c(is.data.frame(df), is.matrix(df), length(dim(df)) == 2)))

  
  # Check columns and select those that will be used to determine distance.
  if (is.null(columns)) {
    dfcoord <- df
  } else {
    stopifnot("All names in 'columns' must match column names in 'df'" = all(columns %in% colnames(df)))
    dfcoord <- df[, columns]
  }


# Define a new operator to choose whether to apply '>' or '>='.
  "%>=>%" <- function(x, y) if(strictly) x > y else x >= y
  
  
  # Computation of distance matrix. Fill diagonal with a very large number.
  dist_df <- as.matrix(dist(dfcoord, method = method, diag = T))
  diag(dist_df) <- max(dist_df) + 1e32
  
  
  # Random shuffle, if set.
  id <- 1:nrow(dfcoord)
  if (shuffle) id <- sample(id)
  j <- id[1]
  id <- id[-1]
  
  
  # Progress bar on screen.
  if (verbose) {
    cat(paste0("\n -> distance_filter: finding points whose distance is > ", min_dist, "...\n"))
    pb <- txtProgressBar(min = 0,
                         max = length(id),
                         style = 3,
                         width = 50,
                         char = "=")
  }


  # Loop through remaining points. We pick them one by one, keeping only the good ones.
  if (verbose) icount <- 0
  for (i in id) {
    
    
    # Progress bar.
    if (verbose) {
      icount <- icount + 1
      setTxtProgressBar(pb, icount)
    }
    
  
    # Check distances.
    k <- c(j, i)
    if (min(dist_df[i, k]) %>=>% min_dist) j <- k
  }

  
  # If no points are found it returns NA.
  if (length(j) == 1) df <- NA else df <- df[j, ]
  
  
  if (verbose) cat("\n")
  
  
  return(df)   
  
}