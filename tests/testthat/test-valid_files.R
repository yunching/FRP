context("valid_files")

test_that("warning about non-existent files", {
  expect_warning(make_filename("a"), "NAs introduced by coercion")
})
