test_that(" Get all the available data about RTN", {
  expect_gt(NROW(get_full_data()),0)
})
