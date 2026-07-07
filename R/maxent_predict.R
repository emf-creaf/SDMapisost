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
maxent_predict <- function(m, x = NULL) {

  # Checks that 'm' is a MaxEnt object or a list of them.
  if (!is(m, "MaxEnt")) cli::cli_abort("Input 'm' must be a MaxEnt object ")


  # Checks that 'x' is a SpatVector object. If 'x' is null, use the data in the model 'm'.
  if (is.null(x)) {
    x <- m@presence
  } else {
    if (!is(x, "SpatVector")) cli::cli_abort("Input 'x' must be a SpatVector")
    x <- as.data.frame(x)
    if (!all(colnames(m@presence) %in% colnames(x))) cli::cli_abort("Name of columns in predictor SpatVector 'x' are wrong")
  }


  # Evaluate MaxEnt at x values.
  pr <- dismo::predict(m, x)


  # # Calculating statistics at each locations.
  # if (length(m) == 1) {
  #   df <- data.frame(mean = pr)
  #   df$sd <- NA
  #   df$IQR <- NA
  #   df$min <- NA
  #   df$max <- NA
  # } else {
  #   df <- data.frame(mean = apply(pr, 1, mean))
  #   df$sd <- apply(pr, 1, sd)
  #   df$IQR <- apply(pr, 1, IQR)
  #   df$min <- apply(pr, 1, min)
  #   df$max <- apply(pr, 1, max)
  # }

  return(pr)

}
