test_that("Multiple models and related functions", {

  # Presence data.
  data(presence_data)
  presence_data <- terra::unwrap(presence_data$`Erica arborea`$selected_data)

  # Load mask.
  data(corine_mask)
  corine_mask <- terra::unwrap(corine_mask)

  # Load rasters.
  data(raster_list_all)
  for (x in names(raster_list_all)) {
    for (y in names(raster_list_all[[x]])) {
      raster_list_all[[x]][[y]] <- terra::unwrap(raster_list_all[[x]][[y]])
    }
  }

  # Select predictors.
  raster_list_selected <- select_raster_list(raster_list_all, names(presence_data))

  # Background points.
  nbackground <- 1000
  background <- generate_random_points(corine_mask, raster_list_selected, nbackground)
  expect_equal(nrow(background), nbackground)

  # Correct fits.
  num_models <- 10
  x <- replicate(num_models, presence_data)
  p <- replicate(num_models, background)
  m <- multiple_models_fit(x, p, verbose = FALSE)
  expect_equal(length(m), num_models)
  test_maxent <- sapply(m, function(z) "MaxEnt" %in% class(z))
  expect_all_true(test_maxent)

  # Wrong fits.
  x <- replicate(num_models, presence_data)
  p <- replicate(7, background)
  expect_error(multiple_models_fit(x, p, verbose = FALSE))

  expect_error(multiple_models_fit(x, p, method = "", verbose = FALSE))
  expect_error(multiple_models_fit(x, p, method = "nothing", verbose = FALSE))
  expect_error(multiple_models_fit(x, p, args = "nothing", verbose = FALSE))
  expect_error(multiple_models_fit(x, p, control = list(), verbose = FALSE))

})
