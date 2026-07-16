test_that("multiplication works", {

  # From Gemini.

  # 1. Fully named list
  list_good <- list(a = 1, b = 2, c = 3)
  expect_true(all_named(list_good))

  # 2. Completely unnamed list
  list_unnamed <- list(1, 2, 3)
  expect_false(all_named(list_unnamed))

  # 3. Partially named list (the trickiest case!)
  list_partial <- list(a = 1, 2, c = 3)
  expect_false(all_named(list_partial))

})
