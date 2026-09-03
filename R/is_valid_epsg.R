# Check for a valid EPSG code.

#' Title
#'
#' @param code \code{character} vector where each element consists of 'epsg:####' or 'EPSG:####' strings,
#' where #### is a valid EPSG numeric code.
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
#' # FALSE
#' is_valid_epsg("3")
#'
#' is_valid_epsg(c("epsg:4326", "epsg:25830", "EPSG:3035", "4326", "epsg:999999"))
is_valid_epsg <- function(code) {

  # Check input.
  if (is.null(code) || length(code) == 0) {
    cli::cli_abort("Input 'code' cannot be NULL or empty.")
  }

  if (!is.character(code)) {
    cli::cli_abort("Input 'code' must be a character vector.")
  }


  # Lower case letters.
  code <- tolower(code)


  # Flag EPSG format
  valid_format <- grepl("^epsg:[0-9]+$", code)


  # Default starting values.
  res <- rep(FALSE, length(code))


  # Check the validity of all codes.
  if (any(valid_format)) {
    res[valid_format] <- vapply(code[valid_format], function(x) {
      crs <- tryCatch(suppressWarnings(terra::crs(x)), error = function(e) "")
      !is.null(crs) && nzchar(crs)},
      logical(1)
    )
  }


  return(res)
}
