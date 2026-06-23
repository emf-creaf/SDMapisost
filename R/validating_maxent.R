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


  # Species.
  folder <- "C:/imidra"
  species <- c("Cistus ladanifer", "Erica arborea", "Genista florida",
               "Quercus coccifera", "Retama sphaerocarpa")


  for (sp in species) {

    # Presence data.
    p <- get_presence(folder, sp)

    # Check that presences are in valid locations. Otherwise, remove.
    p <- cbind(p, terra::extract(corine_mask, p, method = "simple"))
    p <- p[p$category, ]
    p <- p[, 0]

    # Extract predictors at 'p' locations.
    px <- extract_predictors(p, x)

    # Extract background points.
    b <- terra::spatSample(corine_mask, size = nbackground, method = "random", xy = TRUE)

browser()

  }

  min_dist <- 5000
  df <- cbind(as.data.frame(x), terra::crds(x))
  for (i in 1:10) {
    df_filtered <- distance_filter(df, min_dist = min_dist, columns = c("x", "y"), verbose = FALSE)

    m <- dismo::maxent()



  }


}
