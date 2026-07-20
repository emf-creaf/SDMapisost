test_that("multiplication works", {

  # Presence data.
  data(presence_data)
  p <- terra::unwrap(presence_data$`Erica arborea`$selected_data)

  mant <- check_spatial_correlation(p, var_names = names(p), n.class = 40,nperm = 1000)

})
