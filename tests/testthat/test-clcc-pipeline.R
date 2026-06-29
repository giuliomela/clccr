test_that("clcc function returns desired output", {
  data_path <- testthat::test_path("testdata", "example_data")

  clcc_res <- clcc(data_path, price_source = "latest")

  expect_true(is.list(clcc_res))
  expect_false(any(is.na(clcc_res$table$clcc)))
  expect_false(any(is.na(clcc_res$table$share_critical_eu)))
  expect_false(any(is.na(clcc_res$table$share_critical_iea)))
  expect_false(any(clcc_res$table$share_critical_eu > 100))
  expect_false(any(clcc_res$table$share_critical_iea > 100))
})

test_that("clcc function works with previous price scenario", {
  data_path <- testthat::test_path("testdata", "example_data")

  clcc_prev <- clcc(data_path, price_source = "previous")

  expect_true(is.list(clcc_prev))
  expect_false(any(is.na(clcc_prev$table$clcc)))
  expect_false(any(is.na(clcc_prev$table$share_critical_eu)))
})

test_that("clcc function returns desired output if weights are provided", {
  data_path <- testthat::test_path("testdata", "example_data")
  weights_path <- testthat::test_path("testdata", "correzione_coke_silicon.xlsx")

  clcc_res <- clcc(data_path, use_weights = TRUE, weights_path = weights_path)

  expect_true(is.list(clcc_res))
  expect_false(any(is.na(clcc_res$table$share_critical_eu)))
  expect_false(any(is.na(clcc_res$table$share_critical_iea)))
  expect_false(any(clcc_res$table$share_critical_eu > 100))
  expect_false(any(clcc_res$table$share_critical_iea > 100))
})

test_that("clcc_detail works", {
  data_path <- testthat::test_path("testdata", "example_data")

  clcc_detail_res <- clcc_detail(data_path, critical = TRUE, price_source = "latest")

  expect_true(is.list(clcc_detail_res))
  expect_false(any(is.na(clcc_detail_res$table$share)))
})

test_that("clcc_detail works if weights are provided", {
  data_path <- testthat::test_path("testdata", "example_data")
  weights_path <- testthat::test_path("testdata", "correzione_coke_silicon.xlsx")

  clcc_detail_res <- clcc_detail(data_path, use_weights = TRUE, weights_path = weights_path)

  expect_true(is.list(clcc_detail_res))
  expect_false(any(is.na(clcc_detail_res$table$share)))
})

test_that("clcc_mc function returns desired output", {
  data_path <- testthat::test_path("testdata", "example_data")

  clcc_mc_res <- clcc_mc(data_path, rep = 100)
  clcc_mc_det_res <- clcc_mc(data_path, rep = 100, prob_inf_alt = TRUE)

  expect_false(any(clcc_mc_res$table$prob_inf_base > 1))
  expect_identical(sum(is.na(clcc_mc_det_res$table$prob)), length(unique(clcc_mc_det_res$table$obj1)))

  clcc_mc_det_noNA <- subset(clcc_mc_det_res$table, obj1 != obj2)
  expect_false(any(is.na(clcc_mc_det_noNA$prob)))
})

test_that("clcc_mc function returns desired output if weights are provided", {
  data_path <- testthat::test_path("testdata", "example_data")
  weights_path <- testthat::test_path("testpath" = "testdata", "correzione_coke_silicon.xlsx")

  clcc_mc_res <- clcc_mc(data_path, use_weights = TRUE, weights_path = weights_path, rep = 100)
  clcc_mc_det_res <- clcc_mc(data_path, use_weights = TRUE, weights_path = weights_path, rep = 100, prob_inf_alt = TRUE)

  expect_false(any(clcc_mc_res$table$prob_inf_base > 1))
  expect_identical(sum(is.na(clcc_mc_det_res$table$prob)), length(unique(clcc_mc_det_res$table$obj1)))

  clcc_mc_det_noNA <- subset(clcc_mc_det_res$table, obj1 != obj2)
  expect_false(any(is.na(clcc_mc_det_noNA$prob)))
})
