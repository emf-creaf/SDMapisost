validating_maxent <- function(nbackground = 10000) {

  # Check.
  #
  #
  # species <- names(x)
  #
  #
  # for (sp in species) {
  #   y <- x[[sp]]
  #
  # }


  # pxy <- terra::geom(p)
  # df <- as.data.frame(p)
  # # kk <- ncf::correlog(pxy[, "x"],pxy[, "y"], as.matrix(df), increment = 1000)
  # x <- pxy[, "x"]
  # y <- pxy[, "y"]
  # ow <- spatstat.geom::owin(xrange = c(min(x), max(x)), yrange = c(min(y), max(y)))
  # ppp <- spatstat.geom::ppp(x, y, ow)
  # Ripley_L <- spatstat.explore::Kest(ppp)
  # l_envelope <- spatstat.explore::envelope(ppp, fun = spatstat.explore::Kest, nsim = 99, verbose = FALSE)
  # plot(l_envelope)

  # out <- selecting_predictors(save = TRUE)
  out <- base::readRDS("final_predictors.rds")

  # Read predictor data.
  carpetas <- list(mdt = file.path("C:/imidra", "mdt/mdt_madrid.tif"),
                   bioclim = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
                   corine = file.path("C:/imidra", "corine/corine_2018/CORINE Madrid nivel 1.tif"),
                   hidro = file.path("C:/imidra", "hidro/distancia_hidro.tif"))
  names(carpetas$bioclim) <- paste0("bioclim_", 1:19)
  x_all <- get_predictors(carpetas)

  # Mask for valid locations.
  # terra::levels(x$categorical$corine)
  corine_mask <- x_all$categorical$corine == 2 | x_all$categorical$corine == 3
  corine_mask[!corine_mask] <- NA


  # Species.
  folder <- "C:/imidra"
  species <- c("Cistus ladanifer", "Erica arborea", "Genista florida",
               "Quercus coccifera", "Retama sphaerocarpa")

  # Minimum distance for spatial filter.
  min_distance <- c(2) # In Km.
  num_points <- setNames(c(1000), min_distance)

  # Number of simulations.
  num_simu <- 10
  num_null <- 100

  # Proportion of training points.
  train_prop <- .7

  resultados <- list()

  for (sp in species) {

    cli::cli_alert_info(paste0("Species ", sp))

    # Presence data.
    px <- terra::unwrap(out[[sp]]$selected_data)
    px <- terra::na.omit(px, field = "")

    # Remove unneeded predictors for this species.
    # x <- x_all[, names(px)]

    distancia <- list()
    for (mdist in min_distance) {

      cli::cli_alert_info(paste0("   Minimum distance ", mdist, " kilometers"))

      # Extract background points and remove unneeded predictors.
      bx <- generate_random_points(corine_mask, x_all, nbackground)
      bx <- bx[, names(px)]

      # # Remove unneeded predictsor and build data.frames.
      # bx <- bx[, names(px)]
      # dfp <- cbind(as.data.frame(px), terra::crds(px))
      # dfb <- cbind(as.data.frame(bx), terra::crds(bx))

      # Simulation loop.
      auc_index <- boyce_index <- tss_index <- numeric(num_simu)
      auc_null <- boyce_null <- tss_null <- matrix(0, num_simu, num_null)
      results_indices <- list()
      for (simu in 1:num_simu) {

        cli::cli_alert_info(paste0("      Simulation ", simu, ", ",
                                   num_points[as.character(mdist)], " points"))

        # Spatial filter of presence points.
        if (mdist == 0) {
          px_filtered <- px
          bx_filtered <- bx

        } else {
          # px_filtered <- distance_filter(dfp, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)

          browser()
          px_filtered <- spatial_filter(px, min_dist = mdist, crs = "epsg:25830")
          bx_filtered <- spatial_filter(bx, min_dist = mdist, crs = "epsg:25830")


          # Spatial filter of background points.
          # bx_filtered <- distance_filter(dfb, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)
          #
          # while (nrow(bx_filtered) < num_points[as.character(mdist)]) {
          #   cli::cli_alert("         New attempt")
          #   bx_filtered <- distance_filter(dfb, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)
          # }
          # bx_filtered <- bx_filtered[1:num_points[as.character(mdist)], ]
        }

browser()

        # Prepare data.
        np <- nrow(px_filtered)
        nb <- nrow(bx_filtered)
        v <- c(rep(1, np), rep(0, nb))
        df <- rbind(as.data.frame(px_filtered), as.data.frame(bx_filtered))

        # MaxEnt model.
        m <- dismo::maxent(x = subset(df, select = -c(x, y)), p = v)
        pr <- dismo::predict(m, subset(df, select = -c(x, y)))

        # Indices.
        auc_index[simu] <- m@results["Training.AUC", ]
        boyce_index[simu] <- ecospat::ecospat.boyce(pr, pr[1:np], nclass = 20,
                                                    PEplot = FALSE, method = "pearson")$cor
        tss_index[simu] <- ecospat::ecospat.max.tss(pr, v)$max.TSS
        results_indices[[simu]] <- m@results

        ##################################

        # Null model.
        cli::cli_progress_bar("Processing null model", total = num_null)
        for (inull in 1:num_null) {

          cli::cli_progress_update()

          # Random presence points.
          px_rand <- generate_random_points(corine_mask, x, nrow(dfp))
          px_rand <- px_rand[, names(px)]
          dfpx_rand <- cbind(as.data.frame(px_rand), terra::crds(px_rand))

          # Spatial filter.
          dfpx_rand <- distance_filter(dfpx_rand, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)

          # New data.frame.
          df_rand <- rbind(as.data.frame(dfpx_rand), as.data.frame(bx_filtered))

          # Model.
          npr <- nrow(dfpx_rand)
          v <- c(rep(1, npr), rep(0, nb))
          m_null <- dismo::maxent(x = subset(df_rand, select = -c(x, y)), p = v)
          pr_null <- dismo::predict(m_null, subset(df_rand, select = -c(x, y)))

          auc_null[simu, inull] <- m_null@results["Training.AUC", ]
          boyce_null[simu, inull] <- ecospat::ecospat.boyce(pr_null, pr_null[1:npr], nclass = 20,
                                                      PEplot = FALSE, method = "pearson")$cor
          tss_null[simu, inull] <- ecospat::ecospat.max.tss(pr_null, v)$max.TSS

        }
        cli::cli_progress_done()

      }

        distancia[[as.character(mdist)]] <- list(auc = auc_index, boyce = boyce_index, tss = tss_index,
                                                 auc_null = auc_null, boyce_null = boyce_null,
                                                 tss_null = tss_null,
                                                 results_indices = results_indices)
    }

    resultados[[sp]] <- distancia

  }



  saveRDS(resultados, "validating_maxent.rds")

}
