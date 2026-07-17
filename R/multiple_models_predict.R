multiple_models_predict <- function(m, p) {


  # Checks that 'm' or 'p' are lists.
  if (!is.list(m) | !is.list(p)) cli::cli_abort("Inputs 'x' and 'p' must lists")
  if (length(m) != length(p)) {
    cli::cli_abort("Inputs 'm' and 'p' must have the same number of elements")
  }


  # Checks that all elements inside 'm' and 'p' are Maxent and SpatVector objects, respectively.
  if (!all(sapply(m, inherits, "MaxEnt"))) cli::cli_abort("All elements inside input list 'm' must be MaxEnt objects")
  if (!all(sapply(p, inherits, "SpatVector"))) cli::cli_abort("All elements inside input list 'p' must be SpatVector objects")

  # TODO: check that all needed variables names are there.


  # Evaluate at p.
  pr <- sapply(1:length(m), function(i) .model_predict(m[[i]], p[[i]]))

  return(pr)
}
