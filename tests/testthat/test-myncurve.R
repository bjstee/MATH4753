test_that("Probability for a=-3 is correct", {
  list <- myncurve(0, 1, -3)
  actual = list$prob
  expected = round(pnorm(-3, 0, 1), 4)
  expect_setequal(actual, expected) # use expect_setequal to compare values
})

test_that("Probability for a=1 is correct", {
  list <- myncurve(0, 1, 1)
  actual = list$prob
  expected = round(pnorm(1, 0, 1), 4)
  expect_setequal(actual, expected)
})

test_that("Probability for a=3 is correct", {
  list <- myncurve(0, 1, 3)
  actual = list$prob
  expected = round(pnorm(3, 0, 1), 4)
  expect_setequal(actual, expected)
})
