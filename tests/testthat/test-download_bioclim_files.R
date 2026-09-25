test_that("Test downloading bioclim files", {

  resolution <- "10m"

  # Test wrong inputs.
  expect_error(download_bioclim_files())
  expect_error(download_bioclim_files(3, "5"))

  file_to_download <- paste0("https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_", resolution, "_bio.zip")
  destination_folder <- paste0(tempdir(), "_dummy")
  expect_error(download_bioclim_files(file_to_download = file_to_download,
                                      destination_folder = destination_folder,
                                      create_folder = FALSE))


  # Download actual historic data.
  file_to_download <- paste0("https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_", resolution, "_bio.zip")
  destination_folder <- tempdir()
  out <- download_bioclim_files(file_to_download = file_to_download,
                         destination_folder = destination_folder,
                         verbose = FALSE)

  expect_true(length(out) == 19)
  expect_equal(basename(out[1]), paste0("wc2.1_", resolution, "_bio_1.tif"))
  expect_equal(basename(out[19]), paste0("wc2.1_", resolution, "_bio_9.tif"))


  # Download actual future data.
  time_intervals <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")
  resolution <- "10m"
  gcm <- "CMCC-ESM2"
  ssp <- "126"
  base_url <- paste0("https://geodata.ucdavis.edu/cmip6/",
                     resolution, "/",
                     gcm,
                     "/ssp", ssp, "/",
                     "wc2.1_",
                     resolution,
                     "_bioc_",
                     gcm,
                     "_ssp", ssp, "_")
  file_to_download <- paste0(base_url, time_intervals, ".tif")
  destination_folder <- tempdir()
  out <- download_bioclim_files(file_to_download = file_to_download,
                                destination_folder = destination_folder,
                                verbose = FALSE)




})
