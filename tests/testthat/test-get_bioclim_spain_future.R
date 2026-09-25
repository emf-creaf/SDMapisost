test_that("multiplication works", {

  # Lowest resolution. I only need to test.
  resolution <- "10m"

  destination_folder <- tempdir()
  get_bioclim_spain_future(destination_folder = destination_folder,
                             resolution = resolution,
                             ssp = 126,
                             gcm = "CMCC-ESM2")


})
