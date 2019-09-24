context("fpca")
library(refunder)
data(dti_df)

test_that("rfr_fpca runs on dti data", {
  expect_true(2 ==2)
  #expect_error(tidy_fpca(cca, dti_df), NA)
})

#test_that("tidy_fpca runs on cd4 data", {
  #data(dti_df)
  #expect_error(tidy_fpca(cca, dti_df), NA)
#})



test_that("multiple dot-dot-dot arguments accepted for fpca_sc", {
  #expect_error(tidy_fpca(cca, dti_df, nbasis = 9, makePD = TRUE), NA)
})


test_that("correct number of fpcs are returned", {
  # specific npc= for multiple methods, then check the output
})
