#' Title
#'
#' @param mask
#' @param x
#' @param size
#' @param na.remove
#'
#' @returns
#' @export
#'
#' @examples
generate_random_points <- function(mask, x, size = 1000, na.remove = TRUE) {

  # Checks.
  if (!is(mask, "SpatRaster")) cli::cli_abort("Input 'mask' must be a SpatRaster object")
  if (!is.list(x)) cli::cli_abort("Input 'x' must be a list of SpatRaster objects")

  # We should check that objects inside x.!!!!!!


  # Random points across 'mask'.
  p <- terra::spatSample(mask, size = size, method = "random",
                         xy = TRUE, exact = TRUE, as.raster = FALSE)
  p <- terra::vect(p, geom = c("x", "y"), crs = "epsg:25830")

  # Extract data from predictors at locations and remove the 'category' column.
  p <- extract_predictors(p, x, verbose = FALSE)
  p$category <- NULL


  # If selected, remove rows with any NA.
  if (na.remove) p <- terra::na.omit(p, field = "")

  return(p)

}
