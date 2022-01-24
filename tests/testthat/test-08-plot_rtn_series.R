test_that("Generate a line graph for given accounts data", {
  p<- get_year_accumulated_account_data(c("3. RECEITA LÍQUIDA","despesa total","acima da linha"), match_required = FALSE) %>%
    plot_rtn_series(value_type = "2", clean_names = FALSE)

  expect_true(is.ggplot(p))
})
