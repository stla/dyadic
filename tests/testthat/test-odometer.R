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

test_that("Odometer on [0,1[ - forward - one iteration", {
  expect_true(odometer(0)==0.5)
  expect_true(odometer(0.5)==0.25)
  u <- c(0.1, 0.2, 0.3, 0.4)
  Ou <- sapply(u, odometer)
  expect_equal(Ou, u+0.5)
})

test_that("Odometer on [0,1[ - backward - one iteration", {
  expect_true(odometer(0.5, image="backward") == 0)
  expect_true(odometer(0.25, image="backward") == 0.5)
  u <- c(0.6, 0.7, 0.8, 0.9)
  uO <- sapply(u, function(u) odometer(u, image="backward"))
  expect_equal(uO, u-0.5)
})

test_that("Odometer on [0,1[ - several iterations", {
  set.seed(666); u <- runif(1)
  n <- 50L
  Ou <- odometer(u, niters=n)
  expect_true(length(Ou)==n)
  expect_true(all(u < 1) && all(u >= 0))
  uO <- odometer(Ou[n], image="backward", niters=n)
  expect_true(uO[n]==u)
})
