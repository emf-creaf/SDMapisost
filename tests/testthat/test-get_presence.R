test_that("Getting presence data", {


  folder <- "C:/imidra/ifn/nothing.rds"
  species <- "Cistus ladanifer"
  expect_error(get_presence(folder, species, verbose = FALSE))

  folder <- "C:/imidra/ifn/ifn4_28.rds"
  species <- "Fake species"
  x <- get_presence(folder, species, verbose = FALSE)
  expect_equal(nrow(x), 0)

  folder <- "C:/imidra/ifn/ifn4_28.rds"
  species <- "Cistus ladanifer"
  expect_no_condition(x <- get_presence(folder, species, verbose = FALSE))
  expect_true("SpatVector" %in% class(x))


})
