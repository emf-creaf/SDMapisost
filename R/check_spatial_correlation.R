#' Mantel correlograms
#'
#' @description
#' \code{check_spatial_correlation} calculates Mantel correlograms
#'
#'
#' @param p \code{SpatVector} object
#' @param var_names \code{character} vector containing the names of the variables in \code{p}.
#' @param n.class Number of classes (see \code{vegan}).
#' @param nperm Number of permutations for the tests of significance (see \code{vegan}).
#' @param verbose
#'
#' @returns
#' A \code{mantel.correlog} object.
#'
#' @export
#'
#' @examples
check_spatial_correlation <- function(p, var_names = NULL, n.class = 10, nperm = 100) {

  # Checks that 'p' is a SpatVector objects.
  if (!inherits(px, "SpatVector")) cli::cli_abort("Input 'p' must be a SpatVector object")
  if (terra::is.lonlat(p)) {
    if (!grepl("utm", crs(v), ignore.case = TRUE)) cli::cli_abort("Input SpatVector 'p' must be UTM-projected")
  }
  if (!is.null(var_names)) {
    if (!all(var_names %in% names(p))) cli::cli_abort("Names in input 'var_names' do not match names in SpatVector 'p'")
  } else {
    cli::cli_abort("Input 'var_names' must be specified")
  }


  # Prepare data and calculate distance matrices.
  var_mat <- as.data.frame(p)[, var_names]
  test_numeric <- sapply(var_mat, is.numeric)
  if (!all(test_numeric)) cli::cli_abort("At least one of the variables is not numeric")
  var_mat <- as.matrix(var_mat)
  env_dist <- dist(scale(var_mat))


  # Calculate Mantel correlogram.
  mantel_res <- vegan::mantel.correlog(
    D.eco = env_dist,
    XY = terra::crds(p),
    n.class = n.class,
    nperm = nperm
  )

  return(mantel_res)

}
