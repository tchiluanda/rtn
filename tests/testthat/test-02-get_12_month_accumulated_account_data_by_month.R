test_that("Get account data accumulated for 12 months for specifics accounts in specified months", {
  expect_gt(NROW(get_12_month_accumulated_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = NULL)),0)
  expect_gt(NROW(get_12_month_accumulated_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = c(1:12))),0)
  expect_gt(NROW(get_12_month_accumulated_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = c(1:12),  match_required = FALSE)),0)
  expect_gt(NROW(get_12_month_accumulated_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL", month = c(1:12),  match_required = FALSE)),0)
  expect_equal(NROW(get_12_month_accumulated_account_data_by_month(.data= "4. RESULTADO PRIMÁRIO GOVERNO CENTRAL", month = c(1:12),  match_required = FALSE)),0)
})
