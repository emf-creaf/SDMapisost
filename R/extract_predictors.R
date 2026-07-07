#' Extract values from a SpatRaster list.
#'
#' @param p
#' @param raster_list
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- "C:/imidra"
#' species <- "Cistus ladanifer"
#' p <- get_presence(folder, species)
#'
#' # Absolute paths to files.
#' carpetas <- list(mdt = file.path("C:/imidra", "mdt/mdt_madrid.tif"),
#' bioclim = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
#' corine = file.path("C:/imidra", "corine/corine_2018/CORINE Madrid nivel 1.tif"),
#' hidro = file.path("C:/imidra", "hidro/distancia_hidro.tif"))
#'
#' # Name the bioclimatic variables.
#' names(carpetas$bioclim) <- paste0("bioclim_", 1:19)
#'
#' # Read data.
#' x <- get_predictors(carpetas)
#'
#' # Extract predictors at 'p' locations.
#' y <- extract_predictors(p, x)
extract_predictors <- function(p, raster_list, verbose = TRUE) {

  # Checks.
  if (!(any(c("SpatVector", "data.frame") %in% class(p)))) cli::cli_abort("Input 'p' must be a 'terra' object or a 'data.frame'")

  name_elements <- c("terrain", "climate", "distances", "categorical")
  for (i in name_elements) {
    if (!is.null(raster_list[[i]])) {
      if (any(is.null(names(raster_list[[i]])))) cli::cli_abort(paste0("All elements in ", i, " of input list 'raster_list' must have a name"))
    }
  }


  # Extracting predictor data for p locations.
  for (i in name_elements) {
    if (!is.null(raster_list[[i]])) {
      if (verbose) cli::cli_alert_info(paste0(" Extracting ", i, " data"))
      x <- raster_list[[i]]
      for (j in names(x)) {
        if (verbose) cli::cli_alert_info(paste0(" -> ", j))
        method <- ifelse(is.factor(x[[j]]), "simple", "bilinear")
        p[[j]] <- terra::extract(x[[j]], p, ID = FALSE, method = method)
      }
    }
  }


  return(p)

}
