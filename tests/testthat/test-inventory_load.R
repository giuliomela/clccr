test_that("inventory_load actually loads inventories", {

expect_true(tibble::is_tibble(inventory_load_fn("example_data")))

})
