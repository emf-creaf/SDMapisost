multiple_models_fit <- function(x, p, method = "maxent", control = list()) {

  # Checks.
  if (!is(x, "SpatVector") | !is(p, "SpatVector")) cli::cli_abort("Input 'p' must be a SpatVect object")
  if (!setequal(names(x), names(p))) cli::cli_abort("Inputs 'x' and 'p' must have the same columns")
  if (!terra::same.crs(x, p)) cli::cli_abort("Inputs 'x' and 'p' must have the same crs")
  if (!(method %in% c("rf", "maxent"))) cli::cli_abort("Input 'method' must be 'rf' or 'maxent")


  # Default values for the control list.
  if (method == "rf") {
    default_list <- list(num_models = 0,
                         min_dist = 0,
                         num.trees = 500,
                         mtry = 2,
                         importance = "impurity",
                         write.forest = TRUE,
                         probability = FALSE,
                         num.threads = 4)

  } else if (method == "maxent") {
    default_list <- list()
  }
  default_list$min_dist <- 2
  control <- utils::modifyList(default_list, control)


  #
  if (control$min_dist == 0) control$num_models <- 1
  m <- list()
  for (i in 1:control$num_models) {
    if (method == "maxent") {
      if (control$min_dist > 0) {

      }
      m[[i]] <- dismo::maxent()
    }



  }






}
