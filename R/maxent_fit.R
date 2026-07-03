#' Title
#'
#' @param m
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
maxent_fit <- function(x, p) {

  # Checks.
  if (!is(x, "SpatVector") | !is(p, "SpatVector")) cli::cli_abort("Input 'p' must be a SpatVect object")
  if (!setequal(names(x), names(p))) cli::cli_abort("Inputs 'x' and 'p' must have the same columns")
  if (!terra::same.crs(x, p)) cli::cli_abort("Inputs 'x' and 'p' must have the same crs")


  # Prepare data.
  nx <- nrow(x)
  np <- nrow(p)
  v <- c(rep(1, nx), rep(0, np))
  df <- rbind(as.data.frame(x), as.data.frame(p))


  # Run maxent.
  m <- dismo::maxent(x = df, p = v, silent = TRUE)


  return(m)

}
