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
model_fit <- function(x, p, method = "maxent", control = NULL, args = NULL) {

  # Checks.
  if (!is(x, "SpatVector") | !is(p, "SpatVector")) cli::cli_abort("Inputs 'x' or 'p' must be SpatVect objects")
  if (!setequal(names(x), names(p))) cli::cli_abort("Inputs 'x' and 'p' must have the same columns")
  if (!terra::same.crs(x, p)) cli::cli_abort("Inputs 'x' and 'p' must have the same crs")
  if (!(tolower(method) %in% c("rf", "maxent"))) cli::cli_abort("Input 'method' must be 'rf' or 'maxent")

  return(.model_fit(x, p, method, control, args))

}


.model_fit <- function(x, p, method = "maxent", control = NULL, args = NULL) {

  # Default values for the control list.
  method <- tolower(method)
  if (method == "rf") {
    if (!is.null(control)) {
      default_control <- list(num.trees = 500,
                              mtry = 2,
                              importance = "impurity",
                              write.forest = TRUE,
                              probability = FALSE,
                              num.threads = 4)
      control <- utils::modifyList(default_control, control)
    }
  }

  # Prepare data and execute model.
  df <- rbind(as.data.frame(x), as.data.frame(p))
  target <- c(rep(1, nrow(x)), rep(0, nrow(p)))
  if (method == "rf") {
    df$target <- target
    if (is.null(control)) {
      m <- ranger::ranger(target ~ ., data = df)
    } else {
      m <- ranger::ranger(target ~ ., data = df,
                          num.trees = control$num.trees,
                          mtry = control$mtry,
                          importance = control$importance,
                          probability = control$probability,
                          num.threads = control$num.threads)
    }

  } else if (method == "maxent") {
    if (is.null(args)) {
      m <- dismo::maxent(x = df, p = target, silent = TRUE)
    } else {
      m <- dismo::maxent(x = df, p = target, silent = TRUE, args = args)
    }
  }

  return(m)
}
