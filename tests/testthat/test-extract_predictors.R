test_that("Extracting values", {

  # Presence data.
  folder <- "C:/imidra/ifn/ifn4_28.rds"
  species <- "Cistus ladanifer"
  crs <- "EPSG:25830"
  p <- get_presence(folder, species, crs = crs, verbose = FALSE)

  # Absolute paths to files.
  path <- list(terrain = file.path("C:/imidra/mdt/mdt_madrid.tif"),
               climate = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
               categorical = file.path("C:/imidra/corine/corine_2018/CORINE Madrid nivel 1.tif"),
               distances = list(hydro = file.path("C:/imidra/hidro/distancia_hidro.tif")))
  names(path$climate) <- paste0("bioclim_", 1:19)

  crs <- "EPSG:4286"
  x <- get_predictors(path, verbose = FALSE, crs = crs)

  # Input list must have four elements named "terrain", "climate", "distances" and "categorical".
  xx <- x$terrain
  x$terrain <- NULL
  expect_error(extract_predictors(p, x))
  x$terrain <- xx

  # Those elements, though lists themselves, may be empty. If they are not, their elements must be named.
  x$terrain <- xx
  namesxx <- names(xx)
  namesxx[1] <- ""
  names(x$terrain) <- namesxx
  expect_error(extract_predictors(p, x))

  # Wrong input.
  expect_error(extract_predictors(p, 3))

  # Different crs.
  expect_error(extract_predictors(p, x))

  crs <- "EPSG:25830"
  x <- get_predictors(path, verbose = FALSE, crs = crs)
  xx <- x$terrain
  x$terrain <- list()
  expect_no_condition(extract_predictors(p, x, verbose = FALSE))

})
