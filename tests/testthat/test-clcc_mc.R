test_that("clcc_mc function returns desired output", {

data_path <- testthat::test_path("testdata", "example_data")

clcc_mc <- clcc_mc(data_path)

clcc_mc_det <- clcc_mc(data_path, prob_inf_alt = TRUE)

# checking that prob_inf_base is lower than 1
expect_false(any(clcc_mc$table$prob_inf_base > 1))

# checking that prob_inf_alt per the same objects is NA
expect_identical(sum(is.na(clcc_mc_det$table$prob)),
                 length(unique(clcc_mc_det$table$obj1)))

# checking the other values are different from NA
clcc_mc_det_noNA <- subset(clcc_mc_det$table, obj1 != obj2)

expect_false(any(is.na(clcc_mc_det_noNA$prob)))

})


test_that("clcc_mc function returns desired output if weights are provided", {

  data_path <- testthat::test_path("testdata", "example_data")

  weights_path <- testthat::test_path("testdata", "correzione_coke_silicon.xlsx")

  clcc_mc <- clcc_mc(
    data_path,
    T,
    weights_path)

  clcc_mc_det <- clcc_mc(data_path, prob_inf_alt = TRUE)

  # checking that prob_inf_base is lower than 1
  expect_false(any(clcc_mc$table$prob_inf_base > 1))

  # checking that prob_inf_alt per the same objects is NA
  expect_identical(sum(is.na(clcc_mc_det$table$prob)),
                   length(unique(clcc_mc_det$table$obj1)))

  # checking the other values are different from NA
  clcc_mc_det_noNA <- subset(clcc_mc_det$table, obj1 != obj2)

  expect_false(any(is.na(clcc_mc_det_noNA$prob)))

})
