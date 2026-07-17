#' Title
#'
#' @param m
#' @param p
#' @param control
#' @param args
#'
#' @returns
#' @export
#'
#' @examples
model_predict <- function(m, p, control = NULL, args = NULL) {

  # Checks.
  methods <- c("MaxEnt", "ranger")
  if (!(class(m) %in% methods)) cli::cli_abort("Input 'm' must be an accepted model object")
  if (!is(p, "SpatVector")) cli::cli_abort("Input 'p' must be a SpatVect object")
  name_columns <- names(m@presence)
  if (!all(name_columns %in% names(p))) cli::cli_abort("Wrong columns in input 'p'")

  return(.model_predict(m, p, control = control, args = args))


}

.model_predict <- function(m, p, control, args) {

  df <- as.data.frame(p)
  pr <- switch(class(m),
               MaxEnt = dismo::predict(m, df),
               ranger = ranger::predict(m, df)
  )

  return(pr)

}
