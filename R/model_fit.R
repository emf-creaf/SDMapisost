#' Title
#'
#' @param m
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
model_fit <- function(x, p, method = "maxent", control = list()) {

  # Checks.
  if (!is(x, "SpatVector") | !is(p, "SpatVector")) cli::cli_abort("Input 'p' must be a SpatVect object")
  if (!setequal(names(x), names(p))) cli::cli_abort("Inputs 'x' and 'p' must have the same columns")
  if (!terra::same.crs(x, p)) cli::cli_abort("Inputs 'x' and 'p' must have the same crs")
  if (!(method %in% c("rf", "maxent"))) cli::cli_abort("Input 'method' must be 'rf' or 'maxent")


  # Default values for the control list.
  if (method == "rf") {
    default_list <- list(num.trees = 500,
                         mtry = 2,
                         importance = "impurity",
                         write.forest = TRUE,
                         probability = FALSE,
                         num.threads = 4)

  } else if (method == "maxent") {
    default_list <- list()
  }
  control <- utils::modifyList(default_list, control)


  # Prepare data and execute model.
  method <- tolower(method)
  df <- rbind(as.data.frame(x), as.data.frame(p))
  target <- c(rep(1, nrow(x)), rep(0, nrow(p)))
  if (method == "rf") {
    if (length(control) > 0) {
      num.trees <- ifelse(is.null(control[["num.trees"]]), 500, control[["num.trees"]])

    }


    df$target <- target
    m <- ranger::ranger(target ~ ., data = df)

  } else if (method == "maxent") {
    m <- dismo::maxent(x = df, p = target, silent = TRUE)

  }


  return(m)

}
