test_that("clcc_detail works", {

  data_path <- "example_data"

  clcc_res <- clcc_detail(data_path)

  clcc_res_critical <- clcc(data_path, critical = TRUE)

  # output must be a tibble
  expect_true(tibble::is_tibble(clcc_res))

  # No NA expected in the clcc vector
  expect_false(any(is.na(clcc_res$share)))

  # no NA expected in the clcc critical vector
  expect_false(any(is.na(clcc_res_critical$share)))


})
