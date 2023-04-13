
test_that("austria is properly formatted", {
  data(austria)
  # correct type
  expect_type(austria, "list")
  expect_s3_class(austria, c("sf","tbl_df", "tbl", "data.frame"))
  # correct dimensions
  expect_equal(dim(austria), c(1, 3))
  # correct column names
  expect_named(austria, c("GID_0", "NAME_0",  "geometry"))
})
