context("fpca")
library(refunder)

# generate synthetic data where we know the true eigenbasis:
set.seed(1267575)
n <- 100
npc <- 3
argvalues <- seq(0, 1, l = 100)
eigenvalues <- exp(-seq(0, 1, l = npc))
eigenfunctions <- poly(argvalues, npc)
scores <- {
  #generate orthogonal score vectors with desired variance
  raw <- svd(replicate(n, rnorm(npc)))
  t(scale(raw$v)) * sqrt(eigenvalues)
}
data_reg <- 1 + t(eigenfunctions %*% scores) %>% tfd
data_irreg <- data_reg %>% tf_sparsify(dropout = .05)


test_that("rfr_fpca defaults run on regular data", {
  expect_is(rfr_fpca(data_reg), "rfr_fpca")
  reg_fpca <- rfr_fpca(data_reg)

  expect_equivalent(mean(data_reg) %>% tf_evaluations %>% unlist,
                    reg_fpca$mu,
                    tolerance = 0.01 * max(reg_fpca$mu))
  expect_equivalent(reg_fpca$evalues/eigenvalues,
                    rep(1, npc),
                    tolerance = 0.05)
  # abs to remove sign flips
  expect_true(
    mean(abs(abs(reg_fpca$efunctions) - abs(unclass(eigenfunctions)))) <
          mean(abs(eigenfunctions))/10)
  expect_equivalent(
    abs(reg_fpca$scores)/abs(t(scores)),
    matrix(1, n, npc),
    tolerance = 0.05)
})


test_that("rfr_fpca defaults run on irregular data", {
  expect_is(rfr_fpca(data_irreg), "rfr_fpca")
  irreg_fpca <- rfr_fpca(data_irreg)

  expect_equivalent(mean(data_irreg, na.rm = TRUE) %>% tf_evaluations %>% unlist,
                    irreg_fpca$mu,
                    tolerance = 0.01 * max(irreg_fpca$mu))
  expect_equivalent(irreg_fpca$evalues/eigenvalues,
                    rep(1, npc),
                    tolerance = 0.1)
  # abs to remove sign flips
  expect_true(
    mean(abs(abs(irreg_fpca$efunctions) - abs(unclass(eigenfunctions)))) <
      mean(abs(eigenfunctions))/10)
  # more lenient sanity checks for irregular data. thresholds rather arbitrary.
  expect_true(
    mean(abs(irreg_fpca$scores/t(scores)) <  .85) < .15 &
      mean(abs(irreg_fpca$scores/t(scores)) >  1.15) < .15)
})

test_that("multiple dot-dot-dot arguments accepted for fpca_sc", {
  expect_is(rfr_fpca(dti_df$cca, nbasis = 9, makePD = TRUE), "rfr_fpca")
  expect_equal(rfr_fpca(dti_df$cca, nbasis = 5, npc = 3)$npc,
               3)
})

