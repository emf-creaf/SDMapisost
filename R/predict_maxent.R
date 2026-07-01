#' Title
#'
#' @param m
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
#'
predict_maxent <- function(m, x) {

  # Checks.
  if (!is.list(m)) {
    if (!is(m, "MaxEnt")) {
      cli::cli_abort("Input 'm' must be a MaxEnt object or a list of MaxEnt objects")
    }
    m <- list(m)
  }
  else {
    if (!all(sapply(m, is, "MaxEnt"))) cli::cli_abort("Input 'm' must be a MaxEnt object or a list of MaxEnt objects")
  }
  if (!is.data.frame(x)) cli::cli_abort("Input 'x' must be a data.frame containing predictor variables")


  # Evaluate MaxEnt at x values.
  pr <- lapply(m, function(mi) dismo::predict(mi, x))
  pr <- as.data.frame(pr)
  colnames(pr) <- NULL


  # Calculating statistics at each locations.
  if (length(m) == 1) {
    df <- data.frame(mean = pr)
    df$sd <- NA
    df$IQR <- NA
    df$min <- NA
    df$max <- NA
  } else {
    df <- data.frame(mean = apply(pr, 1, mean))
    df$sd <- apply(pr, 1, sd)
    df$IQR <- apply(pr, 1, IQR)
    df$min <- apply(pr, 1, min)
    df$max <- apply(pr, 1, max)
  }

  return(df)

}
