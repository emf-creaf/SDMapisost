#' Title
#'
#' @param path \code{character} string indicating the path to the file with the data..
#' @param species \code{character} string with the full name of the species.
#'
#' @returns
#' An "SpatVector" object containing presence data for the species.
#' @export
#'
#' @examples
#' # Madrid data.
#' path <- "C:/imidra/ifn/ifn4_28.rds"
#' species <- "Erica arborea"
#' x <- get_presence(path, species, "EPSG:25830")
#'
#' # Spanish peninsula data.
#' path <- "C:/imidra/peninsula/ifn/"
#' species <- "Erica arborea"
#' x <- get_presence(path, species, "EPSG:25830")
#'
get_presence <- function(path, species, crs = NULL, verbose = TRUE) {

  # Checks.
  if (!file.exists(path)) cli::cli_abort(paste0("Input path ", folder, " does not exist"))
  if (!(is.character(species) & length(species) == 1)) cli::cli_abort("Input 'species' must be a single species name")
  if (!is.null(crs)) if (!is_valid_epsg(crs)) cli::cli_abort("Wrong 'crs' code or invalid syntax")


  # Read single file, or all files in the folder, and check its/their crs.
  if (file.info(path)$isdir) {
    # Find all .rds files in the folder
    files <- list.files(
      path = path,
      pattern = "\\.rds$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = recursive
    )

    if (length(files) == 0) {
      cli::cli_alert_warning(paste0("No .rds files found in directory: ", path))
      return(list())
    }

    results <- lapply(files, readRDS)
    names(results) <- basename(files)
    return(results)

  } else {
    df <- readRDS(path)
  }


browser()

  df_crs <- paste0("epsg:", df$crs[1])
  if (!is_valid_epsg(df_crs)) cli::cli_abort("Wrong 'crs' code or invalid syntax in IFN data")


  # Retrieve presence data for species.
  if (verbose) cli::cli_alert_info(paste0("Retreaving presence data for ", species))
  species <- tolower(trimws(species))
  x <- df |>
    dplyr::select(id_unique_code, plot, coordx, coordy) |>
    as.data.frame()
  x$species_exists <- sapply(df$understory, function(z) {
    zz <- z$shrub[[1]]
    if (is.data.frame(zz) && nrow(zz) > 0) {
      species %in% tolower(trimws(zz$sp_name))
      } else {
        FALSE
      }
  })
  x <- x[x$species_exists,]
  x$species_exists <- NULL


  # Create SpatVector object.
  x <- terra::vect(x, geom = c("coordx", "coordy"), crs = df_crs)


  # Transform x to crs if needed.
  if (!is.null(crs)) x <- terra::project(x, crs)


  return(x)

}
