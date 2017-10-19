##
## Begin Mamta Sinha code
##
context("Shear Range")

test_that("All input of equal length", {
  c33 <- rnorm(1000, 36, 10)
  c44 <- rnorm(1000, 36, 10)
  epsilon <- rnorm(2000, 1, 0.4)
  gamma   <- rnorm(2000, 1, 0.4)
  delta <- rnorm(1000, 0.3, 0.2)
  expect_error(shearRange(c33, c44, epsilon, gamma, delta), "Input vectors need to be of equal length")
})

test_that("Return value is a dataframe", {
  c33 <- rnorm(1000, 36, 10)
  c44 <- rnorm(1000, 36, 10)
  epsilon <- rnorm(1000, 1, 0.4)
  gamma   <- rnorm(1000, 1, 0.4)
  delta <- rnorm(1000, 0.3, 0.2)
  df <- shearRange(c33, c44, epsilon, gamma, delta)
  expect_is(df, "data.frame")
})

##
## End Mamta Sinha code
##


