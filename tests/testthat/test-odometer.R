context("Odometer")

test_that("Odometer forward - one iteration", {
  d1 <- c(0, 1, 1, 0)
  Od1 <- dyadic:::odometer0.dyadic(d1)
  expect_true(is.dyadic(Od1))
  expect_true(all(Od1 == c(1, 1, 1, 0)))
  d2 <- c(1, 1, 1, 1)
  Od2 <- dyadic:::odometer0.dyadic(d2)
  expect_true(all(Od2 == c(0, 0, 0, 0, 1)))
  #
  expect_identical(odometer.dyadic(d1), Od1)
})
