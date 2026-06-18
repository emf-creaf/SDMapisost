#' Title
#'
#' @param p
#' @param x
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
extract_predictors <- function(p, x, verbose = TRUE) {

  # Checks.
  if (!(any(c("SpatVector", "data.frame") %in% class(p)))) cli::cli_abort("Input 'p' must be a 'terra' object or a 'data.frame'")

  name_elements <- c("terrain", "climate", "distances", "categorical")
  for (i in name_elements) {
    if (!is.null(x[[i]])) {
      if (any(is.null(names(x[[i]])))) cli::cli_abort(paste0("All elements in ", i, " of input list 'x' must have a name"))
    }
  }


  # Extracting predictor data for presence locations.
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
