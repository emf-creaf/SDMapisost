#' Title
#'
#' @param x
#' @param stability
#' @param B
#' @param graph
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
select_variables <- function(x, stability = TRUE, B = 50, graph = TRUE, k = NULL, cutoff = .95, verbose = TRUE) {

  # Checks.
  if (!is.data.frame(x)) x <- as.data.frame(x)
  i <- sapply(x, is.numeric)
  x.quanti <- x[, i, drop = FALSE]
  x.quali <- x[, !i, drop = FALSE]


  # Hierarchical clustering.
  if (verbose) cli::cli_alert_info("Hierarchical clustering of input variables")
  model <- ClustOfVar::hclustvar(X.quanti = x.quanti, X.quali = x.quali)


  # Evaluate stability.
  if (stability) {
    if (verbose) cli::cli_alert_info("Evaluating stability of partitions")
      model_stability <- ClustOfVar::stability(model, B = B, graph = graph)
  }


  # Cutting hierarchical tree at up to half the number of variables.
  if (is.null(k)) {
    k <- round(ncol(x)/2)
  } else {
    if (k >= ncol(x)) {
      cli::cli_abort("Maximum number of clusters must be smaller than number of variables")
    }
  }
  if (verbose) cli_id <- cli::cli_progress_bar("Cutting hierarchical tree", total = k)
  input_index <- 1:k
  model_cut <- lapply(seq_along(input_index), function(i) {
    cli::cli_progress_update(id = cli_id)
    return(ClustOfVar::cutreevar(model, k = i))
  })
  cli::cli_progress_done(id = cli_id)
  model_homogeneity <- sapply(model_cut, function(x) x$E)/100


  # Choose the subset of variables with gain in homogeneity that is just larger than 'cutoff'.
  max_homo <- max(model_homogeneity)
  if (max_homo < .95) {
    cli::cli_abort(paste0("Accounted homogeneity is ", max_homo, ". Please choose a smaller 'cutoff' value"))
  }
  i <- which(model_homogeneity > 0.95)[1]
  model <- model_cut[[i]]


  # Choose the variable per cluster that has the highest correlation with the central component.
  variables <- sapply(names(model$var), function(nam) {
    y <-  model$var[[nam]]
    ifelse(nrow(y) == 1, rownames(y), rownames(y)[which.max(abs(y[,"correlation"]))])
  })


  # Output.
  out <- list(model = model,
              homogeneity = data.frame(k = input_index, accounted = model_homogeneity),
              selected_data = x[, variables])
  if (stability) out$stability <- model_stability
  return(out)


}
