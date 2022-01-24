test_that("Get RTN expense data by year", {
  expect_gt(NROW(get_expense_data(year = 2021)),0)
  expect_gt(NROW(get_expense_data()),0)
})
