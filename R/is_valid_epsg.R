# Check for a valid epsg code.

#' Title
#'
#' @param code \code{character} string of the type 'epsg:####', where #### is a valid EPSG numeric code.
#'
#' @returns
#' TRUE or FALSE.
#'
#' @export
#'
#' @examples
#' is_valid_epsg("epsg:3")
#' is_valid_epsg("epsg:4286")
#'
#' # Error.
#' is_valid_epsg("3")
is_valid_epsg <- function(code) {

  # Check that it contains the substring "epsg:"
  if (is.null(code) | code == "") cli::cli_abort("Input 'code' cannot be NULL or empty")
  code <- tolower(code)
  if (!grepl("epsg:", code)) cli::cli_abort("Input 'code' must have format 'epsg:#####'")

  # Is it valid?
  res <- tryCatch(suppressWarnings(terra::crs(code)), error = function(e) return(""))

  return(res != "")
}
