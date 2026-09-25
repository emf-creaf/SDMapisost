test_that("Test download of bioclim variables", {

  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_"

  # Lowest resolution. I only need to test.
  resolution <- "5m"

  dest_folder <- tempdir()
  get_bioclim_spain(resolution = resolution,
                    dest_folder = dest_folder)


  bioclim_url <- paste0(base_url, resolution, "_bio.zip")


})
