validating_maxent <- function(nbackground = 10000) {


  # Recover predictor selection calculations.
  out <- base::readRDS("final_predictors.rds")

  # Read predictor data.
  carpetas <- list(mdt = file.path("C:/imidra", "mdt/mdt_madrid.tif"),
                   bioclim = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
                   corine = file.path("C:/imidra", "corine/corine_2018/CORINE Madrid nivel 1.tif"),
                   hidro = file.path("C:/imidra", "hidro/distancia_hidro.tif"))
  names(carpetas$bioclim) <- paste0("bioclim_", 1:19)
  raster_list_all <- get_predictors(carpetas)

  # Mask for valid locations.
  corine_mask <- raster_list_all$categorical$corine == 2 | raster_list_all$categorical$corine == 3
  corine_mask[!corine_mask] <- NA

  # Species.
  folder <- "C:/imidra"
  species <- c("Cistus ladanifer", "Erica arborea", "Genista florida",
               "Quercus coccifera", "Retama sphaerocarpa")

  # Minimum distance for spatial filter.
  min_distance <- c(2) # In Km.
  num_points <- setNames(c(1000), min_distance)

  # Number of simulations.
  num_simu <- 20
  num_null <- 100

  # Proportion of training points.
  train_prop <- .7

  resultados <- list()

  for (sp in species) {

    cli::cli_alert_info(paste0("Species ", sp))

    # Presence data.
    presence <- terra::unwrap(out[[sp]]$selected_data)
    presence <- terra::na.omit(presence, field = "")

    # Remove unneeded rasters.
    raster_list_selected <- select_raster_list(raster_list_all, names(presence))

    distancia <- list()
    for (mdist in min_distance) {

      cli::cli_alert_info(paste0("   Minimum distance ", mdist, " kilometers"))

      # Extract background points and remove unneeded predictors.
      background <- generate_random_points(corine_mask, raster_list_selected, nbackground)

      # Simulation loop.
      auc_index <- boyce_index <- tss_index <- numeric(num_simu)
      indices_null <- list()
      results_indices <- list()
      for (simu in 1:num_simu) {

        cli::cli_alert_info(paste0("      Simulation ", simu, ", ",
                                   num_points[as.character(mdist)], " points"))

        # Spatial filter of presence points.
        if (mdist == 0) {
          presence_filtered <- presence
          background_filtered <- background

        } else {
          presence_filtered <- spatial_filter(presence, min_dist = mdist)
          background_filtered <- spatial_filter(background, min_dist = mdist)

        }

        # Prepare data.
        npresence <- nrow(presence_filtered)
        nbackground <- nrow(background_filtered)
        indicator <- c(rep(1, npresence), rep(0, nbackground))

        # Run model and compute prediction.
        m <- maxent_fit(presence_filtered, background_filtered)
        pr <- maxent_predict(m, rbind(presence_filtered, background_filtered))

        # Indices.
        auc_index[simu] <- m@results["Training.AUC", ]
        boyce_index[simu] <- ecospat::ecospat.boyce(pr, pr[1:npresence], nclass = 20,
                                                    PEplot = FALSE, method = "pearson")$cor
        tss_index[simu] <- ecospat::ecospat.max.tss(pr, indicator)$max.TSS
        results_indices[[simu]] <- m@results

        ##################################

        # Indices from null models.
        indices_null[[simu]] <- maxent_null_simu(mask = corine_mask,
                                                 raster_list_selected, npresence, background_filtered, min_dist = mdist)
      }

        distancia[[as.character(mdist)]] <- list(auc = auc_index, boyce = boyce_index, tss = tss_index,
                                                 indices_null = indices_null)
    }

    resultados[[sp]] <- distancia

  }

  saveRDS(resultados, "validating_maxent.rds")

}
