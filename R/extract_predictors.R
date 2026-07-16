#' Extract values from a SpatRaster list.
#'
#' @param p
#' @param x
#' @param verbose \code{logical}, if set to TRUE a progress message is printed on screen.
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
extract_predictors <- function(p, x, verbose = TRUE) {

  # Checks.
  if (!("SpatVector" %in% class(p))) cli::cli_abort("Input 'p' must be a 'SpatVector' object")
  crs <- terra::crs(p)
  name_elements <- c("terrain", "climate", "distances", "categorical")
  if (!all(name_elements %in% names(x))) cli::cli_abort("Wrong elements in input list 'x'")
  for (i in name_elements) {
    y <- x[[i]]
    if (length(y) > 0) {
      if (!all_named(y)) cli::cli_abort(paste0("All elements in the ", i, " element of input list 'x', if they exist, must have a name"))
      for (j in names(y)) {
        if (!terra::same.crs(p, y[[j]])) cli::cli_abort("All elements in 'x' and 'p' must have the same crs")
      }
    }
  }


  # Extracting predictor data for p locations.
  for (i in name_elements) {
    if (!is.null(x[[i]])) {
      if (verbose) cli::cli_alert_info(paste0(" Extracting ", i, " data"))
      y <- x[[i]]
      for (j in names(y)) {
        if (verbose) cli::cli_alert_info(paste0(" -> ", j))
        method <- ifelse(is.factor(y[[j]]), "simple", "bilinear")
        p[[j]] <- terra::extract(y[[j]], p, ID = FALSE, method = method)
      }
    }
  }


  return(p)

}
