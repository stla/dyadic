context("Conversion dyadic - numeric")

test_that("dyadic to numeric", {
 xint <- c(0L, 1L, 0L)
 xnum <- c(0, 1, 0)
 xdyadic <- as.dyadic(xint)
 expect_true(dyadic2num(xint)==1/4)
 expect_true(dyadic2num(xnum)==1/4)
 expect_true(dyadic2num(xdyadic)==1/4)
})

test_that("numeric to dyadic", {
  u <- 1/2+1/4
  d <- num2dyadic(u)
  expect_true(is.dyadic(d))
  expect_true(all(d==c(1L, 1L)))
  expect_identical(names(d), c("0", "1"))
})
