test_that("Valid URLs", {

  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_"

  resolution <- c("30s", "2.5m", "5m", "10m")

  # Test "is_url_valid".
  for (r in resolution) {
    bioclim_url <- paste0(base_url, r, "_bio.zip")
    expect_true(is_valid_url(bioclim_url))

    bioclim_wrong_url <- paste0(base_url, r, ".zip")
    expect_false(is_valid_url(bioclim_wrong_url))
  }

})
