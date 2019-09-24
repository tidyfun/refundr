context("fpca")
library(refunder)
data(dti_df)
data(chf_df)

test_that("rfr_fpca runs on irregular", {
  expect_error(rfr_fpca(dti_df$cca), NA)
})

test_that("rfr_fpca runs on regular data", {
  expect_error(rfr_fpca(chf_df$activity), NA)
})



test_that("multiple dot-dot-dot arguments accepted for fpca_sc", {
  expect_error(rfr_fpca(dti_df$cca, nbasis = 9, makePD = TRUE), NA)
})

