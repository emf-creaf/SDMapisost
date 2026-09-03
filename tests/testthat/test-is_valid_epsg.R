test_that("EPSG codes are valid", {

  expect_false(is_valid_epsg(""))
  expect_error(is_valid_epsg())
  expect_false(is_valid_epsg("eps"))
  expect_error(is_valid_epsg(33))
  expect_false(is_valid_epsg("45"))
  expect_false(is_valid_epsg("epsg:3"))
  expect_true(is_valid_epsg("epsg:4286"))
  expect_true(is_valid_epsg("EPSG:4326"))
  x <- c("epsg:4326", "epsg:25830", "EPSG:3035", "4326", "epsg:999999")
  expect_all_true(is_valid_epsg(x) == c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_error(
    is_valid_epsg(4326),
    "must be a character vector"
  )
  expect_error(is_valid_epsg(character(0)))
  expect_error(is_valid_epsg(NULL))

})
