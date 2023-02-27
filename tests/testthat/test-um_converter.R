test_that("um_converter works", {

  value = 1000

  um_from = "kg"

  um_to = "t"

  res <- um_converter(value, um_from, um_to)

  expect_true(is.numeric(res))

  expect_equal(res, 1)

  expect_error(um_converter(value, "kg", "m"))

  })
