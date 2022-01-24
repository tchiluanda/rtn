test_that("Generates a seasonality graphic for a given account data", {
  p<-
  get_account_data_by_month(c("física"), month = c(1:12), match_required = FALSE) %>%
    plot_seasonality (value_type = "1")

  expect_true(is.ggplot(p))

  expect_error( get_account_data_by_month(c("resultado"), month = c(1:12), match_required = FALSE) %>%
                  plot_seasonality (value_type = "1"))



})
