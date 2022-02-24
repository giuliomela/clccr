test_that("clcc function returns desired output", {

  data_path <- "example_data"

  clcc_res <- clcc(data_path)

  clcc_res_critical <- clcc(data_path, critical = TRUE)

  # output must be a tibble
  expect_true(tibble::is_tibble(clcc_res))

  # No NA expected in the clcc vector
  expect_false(any(is.na(clcc_res$clcc)))

  # no NA expected in the clcc critical vector
  expect_false(any(is.na(clcc_res_critical$share_critical)))

  # clcc critical must be lower than clcc total
  expect_false(any(clcc_res_critical$share_critical > 100))


})
