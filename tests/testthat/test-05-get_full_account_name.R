test_that("Get the full account name related to a given part of an account name", {
  expect_gt(NROW(get_full_account_name(account= "resultado")),0)
  expect_gt(NROW(get_full_account_name()),0)
})
