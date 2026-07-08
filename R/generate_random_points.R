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


  # Random points across 'mask'.
  max_iter <- 10
  iter <- 1
  larger_size <- size
  repeat {

    # Generate random points.
    p <- terra::spatSample(mask, size = larger_size, method = "random",
                           xy = TRUE, exact = TRUE, as.raster = FALSE)

    # Make SpatVector
    p <- terra::vect(p, geom = c("x", "y"), crs = "epsg:25830")

    # Extract data from predictors at locations and remove the 'category' column.
    p <- extract_predictors(p, x, verbose = FALSE)
    p$category <- NULL

    # If selected, remove rows with any NA.
    if (na.remove) p <- terra::na.omit(p, field = "")

    # Check if final size is correct.
    if (nrow(p) < size) {
      iter <- iter + 1
      larger_size <- size * iter
    } else {
      break
    }
    if (iter > max_iter) cli::cli_abort(paste0("Couldn't calculate ", size, " background points"))
  }

  # Check whether there are too many points.
  if (nrow(p) > size) p <- p[1:size, ]

  return(p)

}
