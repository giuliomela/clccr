test_that("clcc_spodumene function returns desired output", {

  data_path <- "example_data"

  clcc_res <- clcc_spodumene(data_path)

  # output must be a list
  expect_true(is.list(clcc_res))

  # No NA expected in the clcc vector
  expect_false(any(is.na(clcc_res$table$clcc)))

  # no NA expected in the clcc critical vector
  expect_false(any(is.na(clcc_res$table$share_critical)))

  # clcc critical must be lower than clcc total
  expect_false(any(clcc_res$table$share_critical > 100))


})

