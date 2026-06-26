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
  x <- get_predictors(carpetas)

  # Mask for valid locations.
  # terra::levels(x$categorical$corine)
  corine_mask <- x$categorical$corine == 2 | x$categorical$corine == 3
  corine_mask[!corine_mask] <- NA


  # Species.
  folder <- "C:/imidra"
  species <- c("Cistus ladanifer", "Erica arborea", "Genista florida",
               "Quercus coccifera", "Retama sphaerocarpa")

  # Minimum distance for spatial filter.
  min_distance <- c(0, 1000)
  num_points <- setNames(c(0, 1000), min_distance)

  # Number of simulations.
  num_simu <- 100
  num_null <- 100

  # Proportion of training points.
  train_prop <- .8

  resultados <- list()

  for (sp in species[2]) {

    cli::cli_alert_info(paste0("Species ", sp))

    # # Presence data.
    # p <- get_presence(folder, sp)
    #
    # # Check that presences are in valid locations. Otherwise, remove.
    # p <- cbind(p, terra::extract(corine_mask, p, method = "simple"))
    # p <- p[p$category, ]
    # p <- p[, 0]
    #
    # # Extract predictors at 'p' locations.
    # px <- extract_predictors(p, x)

    px <- terra::unwrap(out[[sp]]$selected_data)

    distancia <- list()
    for (mdist in min_distance) {

      cli::cli_alert_info(paste0("   Minimum distance ", mdist, " meters"))

      # Extract background points.
      b <- terra::spatSample(corine_mask, size = nbackground, method = "random",
                             xy = TRUE, exact = TRUE, na.rm = TRUE)
      b <- terra::vect(b, geom = c("x", "y"), crs = "EPSG:25830")
      bx <- extract_predictors(b, x, verbose = FALSE)
      bx$category <- NULL

      # Remove NA's.
      px <- terra::na.omit(px, field = "")
      bx <- terra::na.omit(bx, field = "")

      # Remove unneeded predictors and build data.frames.
      bx <- bx[, names(px)]
      dfp <- cbind(as.data.frame(px), terra::crds(px))
      dfb <- cbind(as.data.frame(bx), terra::crds(bx))

      # Simulation loop.
      auc_index <- boyce_index <- pvalue_auc <- pvalue_boyce <- numeric(num_simu)
      for (simu in 1:num_simu) {

        cli::cli_alert_info(paste0("      Simulation ", simu, ", ",
                                   num_points[as.character(mdist)], " points"))

        # Spatial filter of presence points.
        px_filtered <- distance_filter(dfp, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)

        # Spatial filter of background points.
        bx_filtered <- distance_filter(dfb, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)

        while (nrow(bx_filtered) < num_points[as.character(mdist)]) {
          cli::cli_alert("         New attempt")
          bx_filtered <- distance_filter(dfb, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)
        }
        bx_filtered <- bx_filtered[1:num_points[as.character(mdist)], ]

        # Training data.
        ipx <- 1:round(nrow(px_filtered)*train_prop)
        ibx <- 1:round(nrow(bx_filtered)*train_prop)
        v <- c(rep(1, length(ipx)), rep(0, length(ibx)))
        df_train <- rbind(as.data.frame(px_filtered)[ipx,], as.data.frame(bx_filtered)[ibx,])

        # MaxEnt model.
        m <- dismo::maxent(x = df_train, p = v)

        # Test data.
        df_test <- rbind(as.data.frame(px_filtered)[-ipx,], as.data.frame(bx_filtered)[-ibx,])
        pr <- dismo::predict(m, df_test)
        np <- round(nrow(px_filtered)*(1-train_prop))
        z <- c(rep(1, np), rep(0, round(nrow(bx_filtered)*(1-train_prop))))

        # Indices.
        auc_index[simu] <- pROC::auc(z, pr, levels =  c(0, 1), direction = "<")
        boyce_index[simu] <- ecospat::ecospat.boyce(pr, pr[1:np], nclass = 0, PEplot = FALSE, method = "pearson")$cor

        ##################################

        # Null model.
        auc_null <- boyce_null <- numeric(num_null)
        for (inull in 1:num_null) {

          cli::cli_alert_info(paste0("         Null model ", inull, "..."))

          j <- sample(1:length(v))
          m_null <- dismo::maxent(x = df_train, p = v[j])
          pr_null <- dismo::predict(m_null, df_test)
          auc_null[inull] <- pROC::auc(z, pr_null, levels =  c(0, 1), direction = "<")
          boyce_null[inull] <- ecospat::ecospat.boyce(pr_null, pr_null[1:np], nclass = 0,
                                                      PEplot = FALSE, method = "pearson")$cor
        }

        pvalue_auc <- sum(auc_index[simu] < auc_null)/num_null
        pvalue_boyce <- sum(boyce_index[simu] < boyce_index)/num_null

      }

browser()

    }

    distancia[[mdist]] <- list(auc = auc_index, boyce = boyce_index,
                               )

  }

  resultados[[sp]] <- distancia

  save(resultados, "validating_maxent")

}
