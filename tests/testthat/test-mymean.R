test_that("mymean", {
  #issue a warning when x is not numeric
  # and return NA
  x1 <- c(1,2,3,4, "hello")
  expect_warning(mymean(x1))
#  expect_true(is.na(mymean(x1)))

  #return NA when x has NAs and na.rm = F
  x2 <- c(1,2,3,NA)
  expect_true(is.na(mymean(x2, na.rm=FALSE)))

  #return a value equal to sum(x)/length(x) when x has no NA values
  x3 <- c(3, 4, 5, 4+pi, 4-pi)
  expect_equal(mymean(x3), 4.0)

  #return a value equal to sum(x2)/length(x2) where x2 = x[!is.na(x)] if na.rm = T
  expect_equal(mymean(x2, na.rm=TRUE), 2.0)
})
