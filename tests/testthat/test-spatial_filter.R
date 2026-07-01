test_that("multiplication works", {

  # Fetch the official spatial boundary polygon for Spain
  spain_boundary <- suppressMessages(geodata::gadm(country = "ESP", level = 0, path = tempdir()))

  # Randomly sample 100 points strictly within that boundary
  spain_points <- terra::spatSample(spain_boundary, size = 1000, method = "random")

  x <- spatial_filter(spain_points)




})
