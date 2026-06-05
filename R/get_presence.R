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
#' x <- get_presence(folder, species)
#'
get_presence <- function(folder, species, ifn = 2, verbose = TRUE) {

  # Checks.
  nombre_especie <- trimws(species)
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
  x$y <- sapply(df$understory, function(z) {
    zz <- z$shrub[[1]]
    if (is.data.frame(zz) && nrow(zz) > 0) {
      nombre_especie %in% trimws(zz$sp_name)
      } else {
        FALSE
      }
  })


  # Convert to "sf".
  x <- sf::st_as_sf(x,
                    coords = c("coordx", "coordy"),
                    crs = sf::st_crs(paste0("EPSG:", df$crs[1])))


  return(sf::st_geometry(x))

}
