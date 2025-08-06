test_that("start_data_finder returns a numeric value", {

  data_path <- testthat::test_path("testdata", "example_data", "car_diesel.xlsx")

  expect_output(str(start_data_finder_fn(data_path)), "num")

})
