test_that("start_data_finder returns a numeric value", {

  expect_output(str(start_data_finder_fn("car_diesel.xlsx")), "num")

})
