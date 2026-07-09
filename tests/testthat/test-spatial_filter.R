test_that("Spatial filter works.", {


  # Fetch pesence data.
  data(presence_Earborea)
  presence_Earborea <- terra::unwrap(presence_Earborea)

  # Remove unnecessary columns.
  presence_Earborea <- terra::vect(terra::crds(presence_Earborea), type = "points", crs = terra::crs(presence_Earborea))

  # Check several minimum distances.
  min_dist <- c(.1, .5, 1, 2, 5)
  x <- sapply(min_dist, function(x) min(terra::distance(spatial_filter(presence_Earborea, min_dist = x))))

  expect_all_true(x >= min_dist)

})
