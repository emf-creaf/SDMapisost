test_that("Fitting with model_fit", {

  # Presence data.
  data(presence_Earborea)
  presence_Earborea <- terra::unwrap(presence_Earborea)

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
  raster_list_selected <- select_raster_list(raster_list_all, names(presence_Earborea))

  # Background points.
  nbackground <- 1000
  background <- generate_random_points(corine_mask, raster_list_selected, nbackground)
  expect_equal(nrow(background), nbackground)

  # Model fit.
  m <- model_fit(presence_Earborea, background)
  expect_true("MaxEnt" %in% class(m))

})
