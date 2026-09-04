test_that("General tests", {

  expect_error(extract_occurrence(c("as", "asg")))
  expect_error(extract_occurrence("as", 3))
  expect_error(extract_occurrence("as", "b", "nothing"))

})



# What comes below has been implemented by GEMINI AI.
# Helper function to generate mock data matching your exact schema
make_correct_test_rds <- function() {
  temp_file <- tempfile(fileext = ".rds")

  mock_data <- data.frame(
    plot_id = 1:4,
    stringsAsFactors = FALSE
  )

  # 1. "tree" column: list column of data.frames containing "sp_name"
  mock_data$tree <- list(
    data.frame(sp_name = c("Quercus robur", "Pinus sylvestris"), stringsAsFactors = FALSE),
    data.frame(sp_name = c("Fagus sylvatica"), stringsAsFactors = FALSE),
    data.frame(sp_name = c(NA, "Pinus sylvestris"), stringsAsFactors = FALSE),
    data.frame(sp_name = c("QUERCUS ROBUR"), stringsAsFactors = FALSE) # Case test
  )

  # 2. "regen" column: list column of data.frames containing "sp_name"
  mock_data$regen <- list(
    data.frame(sp_name = c("Quercus robur"), stringsAsFactors = FALSE),
    data.frame(other_col = "no_sp_name", stringsAsFactors = FALSE), # Missing sp_name column
    NULL,                                                          # NULL element
    data.frame(sp_name = character(0), stringsAsFactors = FALSE)   # Empty data frame
  )

  # 3. "understory" column: list column of data.frames, each containing "shrub" and "herbs" data.frames
  mock_data$understory <- list(
    # Row 1
    data.frame(
      shrub = I(list(data.frame(sp_name = "Corylus avellana", stringsAsFactors = FALSE))),
      herbs = I(list(data.frame(sp_name = "Rubus fruticosus", stringsAsFactors = FALSE)))
    ),
    # Row 2
    data.frame(
      shrub = I(list(data.frame(sp_name = character(0), stringsAsFactors = FALSE))),
      herbs = I(list(NULL))
    ),
    # Row 3
    data.frame(
      shrub = I(list(data.frame())),
      herbs = I(list(data.frame(sp_name = NA_character_, stringsAsFactors = FALSE)))
    ),
    # Row 4
    data.frame(
      shrub = I(list(NULL)),
      herbs = I(list(NULL))
    )
  )

  saveRDS(mock_data, temp_file)
  return(temp_file)
}


test_that("input validation catches invalid parameters", {
  test_path <- make_correct_test_rds()
  on.exit(unlink(test_path))

  # Path checks
  expect_error(extract_occurrence(path = c("a.rds", "b.rds"), species = "sp"), "Only one path")
  expect_error(extract_occurrence(path = "non_existent.rds", species = "sp"), "Wrong input path")

  # Species checks
  expect_error(extract_occurrence(path = test_path, species = 123), "single species name")
  expect_error(extract_occurrence(path = test_path, species = c("sp1", "sp2")), "single species name")

  # Target checks
  expect_error(extract_occurrence(path = test_path, species = "sp", target = "invalid_target"), "Wrong value for 'target'")
})


test_that("extracts tree occurrence accurately", {
  test_path <- make_correct_test_rds()
  on.exit(unlink(test_path))

  res <- extract_occurrence(path = test_path, species = "Quercus robur", target = "tree", verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true("occurrence" %in% names(res))
  # Row 1: match, Row 2: no match, Row 3: NA ignored/no match, Row 4: case insensitive match
  expect_equal(res$occurrence, c(TRUE, FALSE, FALSE, TRUE))
})


test_that("extracts nested understory (shrub/herbs) occurrence accurately", {
  test_path <- make_correct_test_rds()
  on.exit(unlink(test_path))

  res_shrub <- extract_occurrence(path = test_path, species = "Corylus avellana", target = "shrub", verbose = FALSE)
  expect_equal(res_shrub$occurrence, c(TRUE, FALSE, FALSE, FALSE))

  res_herbs <- extract_occurrence(path = test_path, species = "Rubus fruticosus", target = "herbs", verbose = FALSE)
  expect_equal(res_herbs$occurrence, c(TRUE, FALSE, FALSE, FALSE))
})


test_that("species and target inputs are case- and whitespace-insensitive", {
  test_path <- make_correct_test_rds()
  on.exit(unlink(test_path))

  res_1 <- extract_occurrence(path = test_path, species = "  quercus ROBUR ", target = "TREE", verbose = FALSE)
  res_2 <- extract_occurrence(path = test_path, species = "quercus robur", target = "tree", verbose = FALSE)

  expect_equal(res_1$occurrence, res_2$occurrence)
})


test_that("non-target column options are removed during unnesting", {
  test_path <- make_correct_test_rds()
  on.exit(unlink(test_path))

  res <- extract_occurrence(path = test_path, species = "Quercus robur", target = "shrub", verbose = FALSE)

  expect_false(any(c("tree", "herbs", "regen") %in% names(res)))
})


test_that("edge cases (NULLs, missing sp_name, empty data.frames, NAs) return FALSE without errors", {
  test_path <- make_correct_test_rds()
  on.exit(unlink(test_path))

  expect_no_error(res <- extract_occurrence(path = test_path, species = "Quercus robur", target = "regen", verbose = FALSE))
  expect_type(res$occurrence, "logical")
  expect_false(any(is.na(res$occurrence)))
})


test_that("verbose argument toggles CLI messages", {
  test_path <- make_correct_test_rds()
  on.exit(unlink(test_path))

  expect_message(extract_occurrence(path = test_path, species = "Quercus robur", verbose = TRUE))
  expect_silent(extract_occurrence(path = test_path, species = "Quercus robur", verbose = FALSE))
})
