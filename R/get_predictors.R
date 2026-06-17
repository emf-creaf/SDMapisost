#' Predictor variables at presence locations.
#'
#' @description
#' \code{get_predictors} extracts the value of the different predictor variables
#' that are used in the analysis at all presence locations for the input
#' species.
#'
#' @param folder \code{character} string with the root name of the folder with the data.
#' @param species \code{character} string with the full name of the species.
#' @param verbose \code{logical}, if TRUE progress information is produced.
#'
#' @returns
#' A \code{terra} SpatVector object.
#'
#' @export
#'
#' @examples
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
get_predictors <- function(paths, verbose = TRUE) {

  # # Presence data for species.
  # p <- get_presence(folder, species, verbose = verbose) |>
  #   sf::st_geometry() |>
  #   terra::vect()


  # Elevation and orientation.
  if (verbose) cli::cli_alert_info("Reading 'MDT' data and calculating terrain characteristics")
  mdt <- terra::rast(paths[["mdt"]])
  slope <- terra::terrain(mdt, v = "slope", neighbors = 8, unit = "radians")
  aspect <- terra::terrain(mdt, v = "aspect", neighbors = 8, unit = "radians")
  slope <- tan(slope)*100
  northness <- cos(aspect)
  eastness <- sin(aspect)


  # Bioclimatic variables.
  if (verbose) cli::cli_alert_info("Reading bioclimatic variables")
  x <- paths[["bioclim"]]
  biov <- sapply(names(x), function(i) terra::rast(x[i]))


  # for (i in 1:19) {
  #   biov <- terra::rast(file.path(folder, "bioclim/historico/wc2.1_10m_bio",
  #                                 paste0("wc2.1_10m_bio_", i, ".tif")))
  #   # p <- cbind(p, terra::extract(biov, p, ID = FALSE, method = "bilinear"))
  # }
  # names(p)[-(1:4)] <- paste0("biov", 1:19)


  # CORINE.
  # if (verbose) cli::cli_alert_info("Reading CORINE data")
  # corine <- sf::st_read(file.path(folder, "/corine/corine_2018", "CORINE Madrid nivel 1.tif"),
  #                       quiet = TRUE) |>
  #   terra::vect()
  # p <- cbind(p, terra::extract(corine, p)[, "CODE_18"])
  # names(p)[ncol(p)] <- "corine"
  # clc_classes <- c("artificial", "agricultural", "natural", "wetlands", "water bodies")
  # p$corine <- factor(clc_classes[as.numeric(substr(p$corine, 1, 1))])

  if (verbose) cli::cli_alert_info("Reading CORINE data")
  corine <- terra::rast(paths[["corine"]])


  # Reading raster with distance to rivers.
  if (verbose) cli::cli_alert_info("Reading distance-to-rivers data")
  hidro <- terra::rast(paths[["hidro"]])


  # Collect into a list.
  l <- list(mdt = mdt,
            slope = slope,
            northness = northness,
            eastness = eastness,
            corine = corine,
            hidro = hidro,
            bioclim = biov)


  return(l)

}
