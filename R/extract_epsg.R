#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
extract_epsg <- function(x) {

  if (inherits(x, "SpatRaster")) {
    terra::crs(x, describe = TRUE)$code
  } else if (inherits(x, "sf")) {
      sf::st_crs(x)$epsg
  } else {
    cli::cli_abort("Invalid object type supplied")
  }

}
