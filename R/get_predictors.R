#' Predictor variables at presence locations.
#'
#' @description
#' \code{get_predictors} extracts the value of the different predictor variables
#' that are used in the analysis at all presence locations for the input
#' species.
#'
#' @param path
#' @param crs \code{character} string containing the EPSG code (in the form "EPSG:####) of the
#' coordinate reference system all raster should be projected to. If not given, or set to NULL,
#' no projection will be carried out.
#' #' @param verbose \code{logical}, if TRUE progress information is produced.
#'
#' @returns
#' A \code{terra} SpatRasterCollection object.
#'
#' @export
#'
#' @examples
#' # Absolute paths to files.
#'
#' # Name the bioclimatic variables.
#' names(carpetas$bioclim) <- paste0("bioclim_", 1:19)
#'
#' # Read data.
#' x <- get_predictors(carpetas)
get_predictors <- function(path, crs = NULL, verbose = TRUE) {

  # Checks.
  element_name <- c("terrain", "climate", "categorical", "distances")
  if (!all(element_name %in% names(path))) cli::cli_abort("Wrong elements in input list 'path'")
  if (!is.null(crs)) if (!is_valid_epsg(crs)) cli::cli_abort("Wrong 'crs' code or invalid syntax")


  # Elevation and orientation.
  if (!is.null(path$terrain) | path$terrain != "") {
    if (verbose) cli::cli_alert_info("Reading 'terrain' data and calculating terrain characteristics")
    dem <- terra::rast(path[["terrain"]])
    if (!is.null(crs)) {
      if (!terra::same.crs(dem, crs)) dem <- terra::project(dem, crs, wopt = list(progress = verbose))
    }
    slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "radians")
    aspect <- terra::terrain(dem, v = "aspect", neighbors = 8, unit = "radians")
    slope <- tan(slope)*100
    northness <- cos(aspect)
    eastness <- sin(aspect)
  } else {
    dem <- slope <- northness <- eastness <- NULL
  }


  # Climate variables.
  if (!is.null(path$climate) | all(path$climate != "")) {
    if (verbose) cli::cli_alert_info("Reading 'climate' variables")
    x <- path[["climate"]]
    clim <- sapply(names(x), function(i) {
      y <- terra::rast(x[i])
      if (!is.null(crs)) {
        if (!terra::same.crs(y, crs)) y <- terra::project(y, crs, wopt = list(progress = verbose))
      }
      y
    })
  } else {
    clim <- NULL
  }


  # Categorical (normally, land use/cover) variables.
  if (!is.null(path$categorical) | path$categorical != "") {
    if (verbose) cli::cli_alert_info("Reading categorical data")
    x <- path[["categorical"]]
    categ <- sapply(names(x), function(i) {
      y <- terra::rast(x[[i]])
      if (!is.null(crs)) {
        if (!terra::same.crs(y, crs)) y <- terra::project(y, crs, wopt = list(progress = verbose))
      }
      terra::as.factor(y)
    })
  } else {
    categ <- NULL
  }


  # Reading (multiple) distance variables.
  if (!is.null(path$categorical) | path$categorical != "") {
    if (verbose) cli::cli_alert_info("Reading distance data")
    x <- path[["distances"]]
    distan <- sapply(names(x), function(i) {
      y <- terra::rast(x[[i]])
      if (!is.null(crs)) {
        if (!terra::same.crs(y, crs)) y <- terra::project(y, crs, wopt = list(progress = verbose))
      }
      y
    })
  } else {
    distan <- NULL
  }


  # Collect into a thematic list.
  l <- list(terrain = list(dem = dem,
                           slope = slope,
                           northness = northness,
                           eastness = eastness),
            categorical = categ,
            distances = distan,
            climate = clim)


  return(l)

}
