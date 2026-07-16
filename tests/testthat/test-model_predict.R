test_that("Model fitting", {

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

  # Test model.
  m <- model_fit(presence_data, background)
  expect_true("MaxEnt" %in% class(m))



})
