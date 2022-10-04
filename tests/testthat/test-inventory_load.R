test_that("inventory_load actually loads inventories", {

  data_path <- "example_data"

inventories <- inventory_load_fn(data_path)

# output must be a tibble
expect_true(tibble::is_tibble(inventories))

# All commodities should be present in the master file
test_commodity <- unique(inventories$comm) %in% clccr::clcc_prices_ref$comm

expect_false(any(test_commodity == F)) # if true, at least one commodity is not in the master file

# No NA expected in the quantity vector
expect_false(any(is.na(inventories$quantity)))

})
