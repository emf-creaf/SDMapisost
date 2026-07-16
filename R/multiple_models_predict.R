multiple_models_predict <- function(m, p) {


  # Checks that 'm' or 'p' are lists.
  if (!is.list(x) | !is.list(p)) cli::cli_abort("Inputs 'x' and 'p' must lists")
  if (length(x) != length(p)) {
    cli::cli_abort("Inputs 'x' and 'p' must have the same number of elements")
  }


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
