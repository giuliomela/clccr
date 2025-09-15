test_that("inventory_load actually loads inventories", {

  data_path <- testthat::test_path("testdata", "example_data")

  weights_path <- testthat::test_path("testdata", "correzione_coke_silicon.xlsx")

inventories <- inventory_load_fn(
  data_path = data_path,
  use_weights =  TRUE,
  weights_path =  weights_path)

# output must be a tibble
expect_true(is.data.frame(inventories))

# All commodities should be present in the master file
test_commodity <- unique(inventories$comm) %in% tolower(clccr::clcc_prices_ref$comm)

expect_false(any(test_commodity == F)) # if true, at least one commodity is not in the master file

# No NA expected in the quantity vector
expect_false(any(is.na(inventories$quantity)))

})
