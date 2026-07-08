test_that("Multiple models and related functions", {

  # Look into how utils::modifyList works.

  default_list <- list(num_models = 0,
                       min_dist = 2,
                       num.trees = 500,
                       mtry = 2,
                       importance = "impurity",
                       write.forest = TRUE,
                       probability = FALSE,
                       num.threads = 4)
  input_list <- list()
  final_list <- utils::modifyList(default_list, input_list)

  expect_true(final_list$num_models == 0)





})
