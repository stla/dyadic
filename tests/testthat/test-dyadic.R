context("Dyadic class")

test_that("Dyadic class", {
  xint <- c(0L, 1L, 0L)
  d <- as.dyadic(xint)
  expect_true(class(d)[1]=="dyadic")
  expect_identical(names(d), c("0", "1", "2"))
  xnum <- c(0, 1, 0)
  d <- as.dyadic(xnum)
  expect_true(class(d)[1]=="dyadic")
  expect_identical(as.dyadic(xint), as.dyadic(xnum))
})
