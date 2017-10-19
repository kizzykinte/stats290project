##
## Begin Mamta Sinha code
##
context("Model Fitting")
c33 <- rnorm(3000, 36, 10)
c44 <- rnorm(3000, 36, 10)
epsilon <- rnorm(3000, 1, 0.4)
gamma   <- rnorm(3000, 1, 0.4)
delta <- rnorm(3000, 0.3, 0.2)
df <- shearRange(c33, c44, epsilon, gamma, delta)

test_that("Only lm, rf, gb and all are allowd", {
  expect_error(modelFitting(df, modelFit = "test"), "Only one of lm, rf, gb or all are allowed")
})

test_that("Return list names", {
  expect_identical(names(modelFitting(df)), c("test", "fits", "pred", "testrmse"))
})

test_that("Number of rmse for all fits is 3", {
  expect_equal(length(modelFitting(df, modelFit = "all")$testrmse), 3)
})

##
## End Mamta Sinha code
##
