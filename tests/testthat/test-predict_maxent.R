test_that("multiplication works", {

  # Read predictor data.
  carpetas <- list(mdt = file.path("C:/imidra", "mdt/mdt_madrid.tif"),
                   bioclim = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
                   corine = file.path("C:/imidra", "corine/corine_2018/CORINE Madrid nivel 1.tif"),
                   hidro = file.path("C:/imidra", "hidro/distancia_hidro.tif"))
  names(carpetas$bioclim) <- paste0("bioclim_", 1:19)
  x <- get_predictors(carpetas)

  # Mask for valid locations.
  corine_mask <- x$categorical$corine == 2 | x$categorical$corine == 3
  corine_mask[!corine_mask] <- NA

  # Species.
  folder <- "C:/imidra"
  species <- c("Cistus ladanifer", "Erica arborea", "Genista florida",
               "Quercus coccifera", "Retama sphaerocarpa")

  # Minimum distance for spatial filter.
  min_distance <- c(2000)
  num_points <- setNames(c(1000), min_distance)

  # Number of simulations.
  num_simu <- 10

  # To be used below.
  random_points <- function(mask, x, size) {
    z <- terra::spatSample(mask, size = size, method = "random",
                           xy = TRUE, exact = TRUE, na.rm = TRUE)
    z <- terra::vect(z, geom = c("x", "y"), crs = "EPSG:25830")
    z <- extract_predictors(z, x, verbose = FALSE)
    z$category <- NULL
    z <- terra::na.omit(z, field = "")
    return(z)
  }

  train_prop <- .7

  for (sp in species) {

    # Presence data.
    px <- terra::unwrap(out[[sp]]$selected_data)
    px <- terra::na.omit(px, field = "")

    # Extract background points.
    bx <- random_points(corine_mask, x, nbackground)

    # Remove unneeded predictors and build data.frames.
    bx <- bx[, names(px)]
    dfp <- cbind(as.data.frame(px), terra::crds(px))
    dfb <- cbind(as.data.frame(bx), terra::crds(bx))

    px_filtered <- distance_filter(dfp, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)
    bx_filtered <- distance_filter(dfb, min_dist = mdist, columns = c("x", "y"), verbose = FALSE)


})
