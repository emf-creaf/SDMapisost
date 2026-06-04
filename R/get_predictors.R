#' Predictor variables at presence locations.
#'
#' @description
#' \code{get_predictors} extracts the value of the different predictor variables
#' that are used in the analysis at all presence locations for the input
#' species.
#'
#' @param carpeta \code{character} string
#' @param especie
#'
#' @returns
#' A \code{terra} SpatVector object.
#'
#' @export
#'
#' @examples
#' carpeta <- "C:/imidra/"
#' nombre_especie <- "Cistus ladanifer"
#' df <- get_predictors(carpeta, nombre_especie)
get_predictors <- function(carpeta, especie) {

  # Datos de presencia de la especie.
  p <- datos_especie(file.path(carpeta, "ifn"), nombre_especie) |>
    sf::st_geometry() |>
    terra::vect()


  # Elevación y orientación.
  mdt <- terra::rast(file.path(carpeta, "mdt", "mdt_madrid.tif"))
  slope <- terra::terrain(mdt, v = "slope", neighbors = 8, unit = "radians")
  aspect <- terra::terrain(mdt, v = "aspect", neighbors = 8, unit = "radians")
  slope <- tan(slope)*100
  northness <- cos(aspect)
  eastness <- sin(aspect)

  p <- terra::project(p, terra::crs(mdt))
  p <- cbind(p,
             terra::extract(mdt, p, ID = FALSE, method = "bilinear"),
             terra::extract(slope, p, ID = FALSE, method = "bilinear"),
             terra::extract(northness, p, ID = FALSE, method = "bilinear"),
             terra::extract(eastness, p, ID = FALSE, method = "bilinear"))
  names(p) <- c("mdt", "pendiente", "norte", "este")


  # Variables bioclimáticas.
  for (i in 1:19) {
    biov <- terra::rast(file.path(carpeta, "bioclim/historico/wc2.1_10m_bio",
                                  paste0("wc2.1_10m_bio_", i, ".tif")))
    if (i == 1) p <- terra::project(p, terra::crs(biov))
    p <- cbind(p, terra::extract(biov, p, ID = FALSE, method = "bilinear"))
  }
  names(p)[-(1:4)] <- paste0("biov", 1:19)


  # CORINE.
  corine <- sf::st_read(file.path(carpeta, "/corine/corine_2018", "Madrid_CLC2018.gpkg"),
                        quiet = TRUE) |>
    terra::vect()
  p <- terra::project(p, terra::crs(corine))
  p <- cbind(p, terra::extract(corine, p)[, "CODE_18"])
  names(p)[ncol(p)] <- "corine"

  return(p)

}
