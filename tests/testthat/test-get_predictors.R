test_that("Fetch predictor maps", {

  # Absolute paths to files.
  path <- list(terrain = file.path("C:/imidra/mdt/mdt_madrid.tif"),
                   climate = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
                   categorical = list(corine = file.path("C:/imidra/corine/corine_2018/CORINE Madrid nivel 1.tif")),
                   distances = list(hydro = file.path("C:/imidra/hidro/distancia_hidro.tif")))
  names(path$climate) <- paste0("bioclim_", 1:19)

  # Tests.
  expect_error(get_predictors(path[1]))
  expect_error(get_predictors(path, crs = "epsg:3"))
  crs <- "epsg:4286"
  expect_no_condition(x <- get_predictors(path, verbose = FALSE, crs = crs))
  expect_equal(length(x), 4)

  expect_all_true(sapply(x$terrain, function(y) terra::same.crs(y, crs)))
  expect_all_true(sapply(x$climate, function(y) terra::same.crs(y, crs)))
  expect_all_true(sapply(x$categorical, function(y) terra::same.crs(y, crs)))
  expect_all_true(sapply(x$distances, function(y) terra::same.crs(y, crs)))

})
