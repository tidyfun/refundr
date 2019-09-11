context("fpca")
library(refunder)
library(tidyfun)

test_that("tidy_fpca runs on dti data", {
  data(dti_df)
  expect_error(tidy_fpca(cca, dti_df), NA)
})

#test_that("tidy_fpca runs on cd4 data", {
  #data(dti_df)
  #expect_error(tidy_fpca(cca, dti_df), NA)
#})



test_that("multiple do-dot-dot arguments accepted for fpca_sc", {
  data(dti_df)
  expect_error(tidy_fpca(cca, dti_df, nbasis = 9, makePD = TRUE), NA)
})
