
# Some tests about special preference functionality not tested in test-hasse or test-psel

test_that("Test expression preferences", {
  
  res <- c(30.4, 33.9)
  
  expect_that(psel(mtcars, low_(expression(hp)) * high(mpg))$mpg, equals(res))
  expect_that(psel(mtcars, low_(expression(hp * 2)) * high(mpg))$mpg, equals(res))
  expect_that(psel(mtcars, low(hp) * high_(as.symbol(names(mtcars)[[1]])))$mpg, equals(res))
  
  false <- function(x) -true_(substitute(x))
  expect_that(as.character(false(cyl == 4)), matches("-true(cyl == 4)", fixed = TRUE))
  expect_that(unique(psel(mtcars, false(cyl == 4))$cyl), equals(c(6, 8)))
  
})


test_that("Test length calculation", {
  
  expect_that(length(empty()), equals(0))
  expect_that(length(layered(a, 1, 2)), equals(2))
  expect_that(length(low(a) * -high(b) * empty()), equals(2))  
  expect_that(length(true(a) & true(b)), equals(2))
})