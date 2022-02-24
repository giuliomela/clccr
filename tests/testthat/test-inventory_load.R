test_that("inventory_load actually loads inventories", {

inventories <- inventory_load_fn("example_data")

# output must be a tibble
expect_true(tibble::is_tibble(inventories))

# commodities should be the same as those in the master file
expect_identical(length(unique(inventories$comm)), length(db_comm_master$comm))

# No NA expected in the quantity vector
expect_false(any(is.na(inventories$quantity)))

})
