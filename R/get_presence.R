#' Title
#'
#' @param folder \code{character} string indicating the root folder to retrieve the data from.
#' @param species \code{character} string with the full name of the species.
#' @param ifn \code{numeric} value indicating which IFN to read. It can be equal to 2, 3 or 4.
#'
#' @returns
#' An "sf" object containing presence data for the species.
#' @export
#'
#' @examples
#' folder <- "C:/imidra"
#' species <- "Cistus ladanifer"
#' species <- "Erica arborea"
#' x <- get_presence(folder, species)
#'
get_presence <- function(folder, species, ifn = 4, verbose = TRUE) {

  # Checks.
  nombre_especie <- tolower(trimws(species))
  if (!is.numeric(ifn)) cli::cli_abort("Input parameter 'ifn' must be numeric")
  if (length(ifn) != 1) cli::cli_abort("Input parameter 'ifn' must have length = 1")
  if (!(ifn %in% c(2, 3, 4))) cli::cli_abort("Input parameter 'ifn' must be equal to 2, 3 or 4")


  # Read file for Madrid (code 28).
  df <- switch(as.character(ifn),
               "2" = readRDS(file.path(folder, "ifn", "ifn2_28.rds")),
               "3" = readRDS(file.path(folder, "ifn", "ifn3_28.rds")),
               "4" = readRDS(file.path(folder, "ifn", "ifn4_28.rds"))
  )


  # Retrieve presence data for species.
  if (verbose) cli::cli_alert_info(paste0("Retreaving presence data for species ", species))
  x <- df[, c("id_unique_code", "plot", "coordx", "coordy")]
  x$species_exists <- sapply(df$understory, function(z) {
    zz <- z$shrub[[1]]
    if (is.data.frame(zz) && nrow(zz) > 0) {
      nombre_especie %in% tolower(trimws(zz$sp_name))
      } else {
        FALSE
      }
  })
  x <- x[x$species_exists,]
  x$species_exists <- NULL


  # Convert to "sf".
  x <- sf::st_as_sf(x,
                    coords = c("coordx", "coordy"),
                    crs = sf::st_crs(paste0("EPSG:", df$crs[1])))


  # Transform to EPSG:25830.
  x <- sf::st_transform(x, crs = "EPSG:25830")


  # Switch to a SpatVector object.
  x <- x |> sf::st_geometry() |> terra::vect()


  return(x)

}
