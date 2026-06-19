#' Variable selection with hierarchical clustering
#'
#' @description
#' \code{select_variables} uses hierarchical clustering from package \code{ClustOfVar}
#' to select best subset of variables.
#'
#' @param x \code{data.frame} or \code{sf} object containing a set of variables.
#' @param stability \code{logical} variable, if set to TRUE stability of partition is calculated.
#' @param B \code{numeric}, number of bootstrap samples.
#' @param verbose \code{logical} if set to TRUE, information about progress is printed.
#'
#' @returns
#' A \code{list} containing the final model, a \code{data.frame} with the percentage of homogeneity
#' that is accounted by each number of clusters and the subset of data variables. If \code{stability}
#' is set to TRUE the output of the stability analysis is added.
#' @export
#'
#' @examples
#' # All numeric data.
#' data(decathlon, package = "ClustOfVar")
#' subset <- select_variables(decathlon, cutoff = 0.95)
#' plot(subset$homogeneity, xlab = "N. of clusters", ylab = "Gain in homogeneity")
#' points(c(0, 13), c(.95, .95), type = "l", lty = 2, lwd = 2)
#'
#' # All categorical data.
#' data(vnf, package = "ClustOfVar")
#' subset <- select_variables(vnf, cutoff = 0.95)
#' plot(subset$homogeneity, xlab = "N. of clusters", ylab = "Gain in homogeneity")
#' points(c(0, 13), c(.95, .95), type = "l", lty = 2, lwd = 2)
#'
#'
#' folder <- "C:/imidra"
#' species <- "Cistus ladanifer"
#' p <- get_presence(folder, species)
#'
#' # Absolute paths to files.
#' carpetas <- list(mdt = file.path("C:/imidra", "mdt/mdt_madrid.tif"),
#' bioclim = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
#' corine = file.path("C:/imidra", "corine/corine_2018/CORINE Madrid nivel 1.tif"),
#' hidro = file.path("C:/imidra", "hidro/distancia_hidro.tif"))
#'
#' # Name the bioclimatic variables.
#' names(carpetas$bioclim) <- paste0("bioclim_", 1:19)
#'
#' # Read data.
#' x <- get_predictors(carpetas)
#'
#' # Extract predictors at 'p' locations.
#' p <- extract_predictors(p, x)
#'
#' # Variable selection.
#' subset <- select_variables(p, cutoff = 0.95)
select_variables <- function(x, stability = TRUE, B = 50, k = NULL, cutoff = NULL, verbose = TRUE) {

  # Checks.
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (is.null(cutoff)) cutoff <- .95


  # Hierarchical clustering.
  if (verbose) cli::cli_alert_info("Hierarchical clustering of input variables")
  i <- sapply(x, is.numeric)
  if (sum(i) == 0) {                              # All qualitative.
    model <- ClustOfVar::hclustvar(X.quali = x)
  } else if (ncol(x) == sum(i)) {                 # All quantitative.
    model <- ClustOfVar::hclustvar(X.quanti = x)
  } else {
    model <- ClustOfVar::hclustvar(X.quanti =  x[, i, drop = FALSE], X.quali = x[, !i, drop = FALSE])
  }


  # Evaluate stability.
  if (stability) {
    if (verbose) cli::cli_alert_info("Evaluating stability of partitions")
      model_stability <- ClustOfVar::stability(model, B = B, graph = FALSE)
  }


  # Cutting hierarchical tree.
  if (is.null(k)) k <- ncol(x)
  if (verbose) cli_id <- cli::cli_progress_bar("Cutting hierarchical tree", total = k)
  input_index <- 1:k
  model_cut <- lapply(seq_along(input_index), function(i) {
    if (verbose) cli::cli_progress_update(id = cli_id)
    return(ClustOfVar::cutreevar(model, k = i))
  })
  cli::cli_progress_done(id = cli_id)
  model_homogeneity <- sapply(model_cut, function(x) x$E)/100


  # Choose the subset of variables with gain in homogeneity that is just larger than 'cutoff'.
  max_homo <- max(model_homogeneity)
  if (max_homo < cutoff) {
    cli::cli_abort(paste0("Accounted homogeneity is ", max_homo, ". Please choose a smaller 'cutoff' value"))
  }
  i <- which(model_homogeneity > cutoff)[1]
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
