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

test_that("Odometer backward - one iteration", {
  d1 <- c(1, 1, 1, 0)
  Od1 <- dyadic:::odometer0.dyadic(d1, image="backward")
  expect_true(is.dyadic(Od1))
  expect_true(all(Od1 == c(0, 1, 1, 0)))
  d2 <- c(0, 0, 0, 0)
  Od2 <- dyadic:::odometer0.dyadic(d2, image="backward")
  expect_true(all(Od2 == c(1, 1, 1, 1, 0)))
  #
  expect_identical(odometer.dyadic(d1, image="backward"), Od1)
})

test_that("Odometer forward - two iterations", {
  d <- c(0, 1, 1, 0)
  Od1 <- dyadic:::odometer0.dyadic(d)
  Od2 <- dyadic:::odometer0.dyadic(Od1)
  Od12 <- odometer.dyadic(d, niters=2)
  expect_true(all(Od12 == rbind(Od1, Od2)))
})


test_that("Odometer backward - two iterations", {
  d <- c(0, 1, 1, 0)
  Od1 <- dyadic:::odometer0.dyadic(d, image="backward")
  Od2 <- dyadic:::odometer0.dyadic(Od1, image="backward")
  Od12 <- odometer.dyadic(d, niters=2, image="backward")
  expect_true(all(Od12 == rbind(Od1, Od2)))
})
