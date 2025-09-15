test_that("clcc_detail works", {

  data_path <- testthat::test_path("testdata", "example_data")

  clcc_detail <- clcc_detail(data_path, critical = TRUE)

  # output must be a list
  expect_true(is.list(clcc_detail))

  # No NA expected in the clcc vector
  expect_false(any(is.na(clcc_detail$table$share)))

  # no NA expected in the clcc critical vector
  expect_false(any(is.na(clcc_detail$table$share)))


})

test_that("clcc_detail works id weights are provided", {

  data_path <- testthat::test_path("testdata", "example_data")

  weights_path <- testthat::test_path("testdata", "correzione_coke_silicon.xlsx")

  clcc_detail <- clcc_detail(
    data_path,
    T,
    weights_path)

  # output must be a list
  expect_true(is.list(clcc_detail))

  # No NA expected in the clcc vector
  expect_false(any(is.na(clcc_detail$table$share)))

  # no NA expected in the clcc critical vector
  expect_false(any(is.na(clcc_detail$table$share)))


})
