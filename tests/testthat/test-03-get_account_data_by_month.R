test_that("Get data for specifics accounts in specified months", {
  expect_gt(NROW(get_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = 12)),0)
  expect_gt(NROW(get_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = c(1:12))),0)
  expect_gt(NROW(get_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = 12, match_required = FALSE)),0)
  expect_gt(NROW(get_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL",match_required = FALSE, month = 12)),0)
  expect_equal(NROW(get_account_data_by_month(.data= "4. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)",match_required = FALSE, month = 12)),0)
})
