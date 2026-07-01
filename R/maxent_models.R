#' Title
#'
#' @param m
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
maxent_models <- function(m, x, min_dist = 2000, nbackground = 1000) {

  p <- lapply(m, function(ma) dismo::predict(ma, x))

  # Mean value and standard deviation.

}
