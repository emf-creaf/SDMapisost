#' Title
#'
#' @param x
#' @param p
#' @param method
#' @param control
#' @param args
#'
#' @returns
#' @export
#'
#' @examples
multiple_models_fit <- function(x, p, method = "maxent", control = NULL, args = NULL, verbose = TRUE) {

  # Checks.
  if (!is.list(x) | !is.list(p)) cli::cli_abort("Inputs 'x' and 'p' must lists")
  if (length(x) != length(p)) {
    cli::cli_abort("Inputs 'x' and 'p' must have the same number of elements")
  }
  num_models <- length(x)
  for (i in 1:num_models) {
    if (!is(x[[i]], "SpatVector") | !is(p[[i]], "SpatVector"))
      cli::cli_abort("All elements in input lists 'x' and 'p' must be SpatVect objects")

    if (!setequal(names(x[[i]]), names(p[[i]])))
      cli::cli_abort("All elements in input lists 'x' and 'p' must have the same columns")

    if (!(terra::same.crs(x[[i]], p[[i]]) & terra::same.crs(x[[1]], p[[i]])))
      cli::cli_abort("All elements in input lists 'x' and 'p' must have the same crs")

  }
  if (!(tolower(method) %in% c("rf", "maxent"))) cli::cli_abort("Input 'method' must be 'rf' or 'maxent")


  # List of models.
  if (verbose) cli_id <- cli::cli_progress_bar(paste0("Fitting ", method, " models"), total = num_models)
  m <- list()
  for (i in 1:num_models) {
    if (verbose) cli::cli_progress_update(id = cli_id)
    m[[i]] <- .model_fit(x[[i]], p[[i]], method = method, control = control, args = args)
  }
  if (verbose) cli::cli_progress_done(id = cli_id)


  return(m)
}
