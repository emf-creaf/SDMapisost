test_that("EPSG codes are valid", {

  expect_error(is_valid_epsg(""))
  expect_error(is_valid_epsg())
  expect_error(is_valid_epsg("eps"))
  expect_error(is_valid_epsg(33))
  expect_error(is_valid_epsg("45"))
  expect_false(is_valid_epsg("epsg:3"))
  expect_true(is_valid_epsg("epsg:4286"))

})
