test_that("Bioclim historic data", {

  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_"

  # Lowest resolution. I only need to test.
  resolution <- "10m"

  destination_folder <- tempdir()
  get_bioclim_spain_historic(destination_folder = destination_folder,
                             resolution = resolution)


  bioclim_url <- paste0(base_url, resolution, "_bio.zip")

})
