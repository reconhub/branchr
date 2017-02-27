
test_that("check_alpha works", {

  skip_on_cran()

  msg <- "alpha must be exatly 1 value"
  expect_error(check_alpha(1:2), msg)

  msg <- "alpha must be strictly positive"
  expect_error(check_alpha(-.1), msg)

})
