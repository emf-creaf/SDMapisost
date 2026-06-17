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


  # Extract topographic data.
  if (verbose) cli::cli_alert_info("Extracting terrain characteristics")
  if (!is.null(x$mdt)) p$mdt <- terra::extract(x$mdt, p, ID = FALSE, method = "bilinear")
  if (!is.null(x$slope)) p$slope <- terra::extract(x$slope, p, ID = FALSE, method = "bilinear")
  if (!is.null(x$northness)) p$northness <- terra::extract(x$northness, p, ID = FALSE, method = "bilinear")
  if (!is.null(x$eastness)) p$eastness <- terra::extract(x$eastness, p, ID = FALSE, method = "bilinear")


  # Extract bioclimatic data.
  if (!is.null(x$bioclim)) {
    if (verbose) cli::cli_alert_info("Extracting bioclimatic variables")
    for (i in names(x$bioclim)) p[[i]] <- terra::extract(x$bioclim[[i]], p, ID = FALSE, method = "bilinear")
  }


  # Extract land use data.
  if (!is.null(x$corine)) {
    if (verbose) cli::cli_alert_info("Extracting land use variables")
    p$corine <- terra::extract(x$corine, p, ID = FALSE)
  }


  # Extract distance to hidrographic network data.
  if (!is.null(x$hidro)) {
    if (verbose) cli::cli_alert_info("Extracting distance to hidrographic network variables")
    p$hidro <- terra::extract(x$hidro, p, ID = FALSE)
  }


  return(p)

}
