test_that("Get RTN revenue data by year", {
  expect_gt(NROW(get_revenue_data(year = 2021)),0)
  expect_gt(NROW(get_revenue_data()),0)
  expect_gt(NROW(get_revenue_data(year = c(2020,2021))),0)
  expect_equal(NROW(get_revenue_data(year = 1972)),0)
})
