test_that("clcc function returns desired output", {

  data_path <- testthat::test_path("testdata", "example_data")

  clcc_res <- clcc(data_path)

  # output must be a list
  expect_true(is.list(clcc_res))

  # No NA expected in the clcc vector
  expect_false(any(is.na(clcc_res$table$clcc)))

  # no NA expected in the EU clcc critical vector
  expect_false(any(is.na(clcc_res$table$share_critical_eu)))

  # no NA expected in the IEA clcc critical vector
  expect_false(any(is.na(clcc_res$table$share_critical_iea)))

  # EU clcc critical must be lower than clcc total
  expect_false(any(clcc_res$table$share_critical_eu > 100))

  # IEA clcc critical must be lower than clcc total
  expect_false(any(clcc_res$table$share_critical_iea > 100))


})

test_that("clcc function returns desired output if weights are provided", {

  data_path <- testthat::test_path("testdata", "example_data")

  weights_path <- testthat::test_path("testdata", "correzione_coke_silicon.xlsx")

  clcc_res <- clcc(
    data_path,
    T,
    weights_path)

  # output must be a list
  expect_true(is.list(clcc_res))

  # no NA expected in the EU clcc critical vector
  expect_false(any(is.na(clcc_res$table$share_critical_eu)))

  # no NA expected in the IEA clcc critical vector
  expect_false(any(is.na(clcc_res$table$share_critical_iea)))

  # EU clcc critical must be lower than clcc total
  expect_false(any(clcc_res$table$share_critical_eu > 100))

  # IEA clcc critical must be lower than clcc total
  expect_false(any(clcc_res$table$share_critical_iea > 100))


})
