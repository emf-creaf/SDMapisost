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
#' folder <- "C:/imidra/"
#' species <- "Cistus ladanifer"
#' x <- get_predictors(folder, species)
get_predictors <- function(folder, species, verbose = TRUE) {

  # Presence data for species.
  p <- get_presence(folder, species, verbose = verbose) |>
    sf::st_geometry() |>
    terra::vect()


  # Elevation and orientation.
  mdt <- terra::rast(file.path(folder, "mdt", "mdt_madrid.tif"))

  slope <- terra::terrain(mdt, v = "slope", neighbors = 8, unit = "radians")
  aspect <- terra::terrain(mdt, v = "aspect", neighbors = 8, unit = "radians")
  slope <- tan(slope)*100
  northness <- cos(aspect)
  eastness <- sin(aspect)


  if (verbose) cli::cli_alert_info("Extracting terrain characteristics at presence locations")
  p <- terra::project(p, terra::crs(mdt))
  p <- cbind(p,
             terra::extract(mdt, p, ID = FALSE, method = "bilinear"),
             terra::extract(slope, p, ID = FALSE, method = "bilinear"),
             terra::extract(northness, p, ID = FALSE, method = "bilinear"),
             terra::extract(eastness, p, ID = FALSE, method = "bilinear"))
  names(p) <- c("mdt", "pendiente", "norte", "este")


  # Bioclimatic variables.
  if (verbose) cli::cli_alert_info("Extracting bioclimatic data at presence locations")
  for (i in 1:19) {
    biov <- terra::rast(file.path(folder, "bioclim/historico/wc2.1_10m_bio",
                                  paste0("wc2.1_10m_bio_", i, ".tif")))
    if (i == 1) p <- terra::project(p, terra::crs(biov))
    p <- cbind(p, terra::extract(biov, p, ID = FALSE, method = "bilinear"))
  }
  names(p)[-(1:4)] <- paste0("biov", 1:19)


  # CORINE.
  if (verbose) cli::cli_alert_info("Extracting CORINE data at presence locations")
  corine <- sf::st_read(file.path(folder, "/corine/corine_2018", "Madrid_CLC2018.gpkg"),
                        quiet = TRUE) |>
    terra::vect()
  p <- terra::project(p, terra::crs(corine))
  p <- cbind(p, terra::extract(corine, p)[, "CODE_18"])
  names(p)[ncol(p)] <- "corine"
  clc_classes <- c("artificial", "agricultural", "natural", "wetlands", "water bodies")
  p$corine <- factor(clc_classes[as.numeric(substr(p$corine, 1, 1))])


  return(p)

}
