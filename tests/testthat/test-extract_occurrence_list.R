test_that("multiplication works", {

  filepath <- paste0(testthat::test_path("data"), "/", c("ifn4_01.rds", "ifn4_20.rds", "ifn4_39.rds", "ifn4_50.rds"))
  species <- "Erica arborea"

  # Tests for 1 and for 4 files.
  for (i in c(1, 4)) {

    path <- filepath[1:i]

    # Error. merge has been set to TRUE but no input 'crs' has been provided.
    expect_error(extract_occurrence_list(path, species, "shrub", merge = TRUE, verbose = F))

    # Expectation is that output is 4-element list containing data.frames
    x <- extract_occurrence_list(path, species, "shrub", merge = FALSE, verbose = F)
    lapply(x, expect_s3_class, "data.frame")

    # Expectation is that output is a 4-element list containing SpatVector objects.
    x <- extract_occurrence_list(path, species, "shrub", merge = FALSE, crs = "EPSG:4326", verbose = F)
    lapply(x, expect_s4_class, "SpatVector")

    # Expectation is that output is a single SpacVector object and its number of rows
    # is the same as the total number of rows of elements from the previous list 'x'.
    y <- extract_occurrence_list(path, species, "shrub", merge = TRUE, crs = "EPSG:4326", verbose = F)
    expect_equal(nrow(y), sum(sapply(x, nrow)))

    # Same 'crs'.
    expect_true(all(sapply(x, function(z) terra::crs(z) == terra::crs(y))))

    # Same column names.
    expect_true(all(sapply(x, function(z) all(terra::names(z) == terra::names(y)))))

  }

})
