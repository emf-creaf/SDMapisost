#' Title
#'
#' @param mask
#' @param x
#' @param np \code{numeric} number of presence points.
#' @param bx spatially filtered background SpatVector object.
#' @param min_dist
#' @param num_null
#' @param size
#' @param verbose

#'
#' @returns
#' @export
#'
#' @examples
maxent_null_simu <- function(mask, x, num_presence, background, min_dist = 20, num_null = 20, size = 1000, verbose = TRUE) {

  # Checks.
  if (size <= num_presence) size <- num_presence * 10

  # Simulation loop.
  auc <- boyce <- tss <- numeric(num_null)

  if (verbose) cli::cli_progress_bar("Processing null model", total = num_null)
  for (i in 1:num_null) {

    if (verbose) cli::cli_progress_update()

    # Random presence points.
    prand <- generate_random_points(mask, x, size)

    # Filtering spatial data.
    prand <- spatial_filter(prand, min_dist = min_dist)

    # Downsize presence data.
    if (nrow(prand) > num_presence) {
      j <- sample(nrow(prand))[1:num_presence]
      prand <- prand[j, ]
    }


    # MaxEnt model.
    m <- maxent_fit(prand, background)
    pr <- maxent_predict(m, rbind(prand, background))


    # Save indices.

    auc[i] <- m@results["Training.AUC", ]
    boyce[i] <- ecospat::ecospat.boyce(pr, pr[1:num_presence], nclass = 20,
                                                      PEplot = FALSE, method = "pearson")$cor
    v <- c(rep(1, num_presence), rep(0, nrow(background)))
    tss[i] <- ecospat::ecospat.max.tss(pr, v)$max.TSS

  }

  return(data.frame(auc = auc, boyce = boyce, tss = tss))

}
