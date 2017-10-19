##
## Begin Mamta Sinha code
##
context("Generate Data")

test_that("Default generateData produces 6 column dataframe", {
  expect_equal(ncol(generateData()), 6)
  expect_message(generateData(), "shearRange")
})

test_that(" Default uses model = shearRange", {
  expect_message(generateData(), "shearRange")
})

test_that("Passing different lengths of vectors of mean and sd", {
  expect_error(generateData(v_mean = list(10, 20, 30), v_sd = list(2, 3, 4, 5)), 
               "The number of input means have to be equal to the number of sd")
})

v_mean = list(36, 36, 1, 1, 0.3)
v_sd = list(10, 10, 0.4, 0.4, 0.2)
v_i <- mapply(rnorm, 2000, v_mean, v_sd)

test_that("User data is getting used", {
  expect_message(generateData(userdata = v_i), "Using user specified data")
})

##
## End Mamta Sinha code
##


