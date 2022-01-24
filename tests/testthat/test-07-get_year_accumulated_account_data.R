test_that("Get account data accumulated in a year for specifics accounts in specified months", {
  expect_gt(NROW(get_year_accumulated_account_data(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)")),0)
  expect_gt(NROW(get_year_accumulated_account_data(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", match_required = FALSE)),0)
  expect_gt(NROW(get_year_accumulated_account_data(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL", match_required = FALSE)),0)
  expect_equal(NROW(get_year_accumulated_account_data(.data= "4. RESULTADO PRIMÁRIO GOVERNO CENTRAL", match_required = TRUE)),0)
  expect_equal(NROW(get_year_accumulated_account_data(.data= "4. RESULTADO PRIMÁRIO GOVERNO CENTRAL", match_required = FALSE)),0)
})
