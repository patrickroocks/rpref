
# Some tests about special preference functionality not tested in test-hasse or test-psel


test_that("Test expression output of preference", {
  
  expect_that(as.character(as.expression(low(2*a) * -low(b))), 
              matches("low(2 * a) * reverse(low(b))", fixed = TRUE))
  
})


test_that("Test inherit functions", {
  
  expect_that(is.preference(empty()), equals(TRUE))
  expect_that(is.empty_pref(empty()), equals(TRUE))
  expect_that(is.base_pref(empty()), equals(FALSE))
  
  expect_that(is.base_pref(low(a) & empty()), equals(TRUE))
  
  expect_that(is.complex_pref(low(a) & low(b)), equals(TRUE))
})


test_that("Test expression preferences", {
  
  res <- c(30.4, 33.9)
  
  expect_that(psel(mtcars, low_(expression(hp)) * high(mpg))$mpg, equals(res))
  expect_that(psel(mtcars, low_(expression(hp * 2)) * high(mpg))$mpg, equals(res))
  expect_that(psel(mtcars, low(hp) * high_(as.symbol(names(mtcars)[[1]])))$mpg, equals(res))
  
  false <- function(x) -true_(substitute(x))
  expect_that(as.character(false(cyl == 4)), matches("-true(cyl == 4)", fixed = TRUE))
  expect_that(unique(psel(mtcars, false(cyl == 4))$cyl), equals(c(6, 8)))
  
})


test_that("Test preferences with df__", {
  
  expect_that(psel(mtcars, low(df__[[3]]))$disp, equals(71.1))
  expect_that(psel(mtcars, high(df__[[4]]))$hp, equals(335))
  expect_that(psel(mtcars, true(rownames(df__) == "Fiat 128"))$mpg, equals(32.4))
  
})


test_that("Test length calculation", {
  
  expect_that(length(empty()), equals(0))
  expect_that(length(layered(a, 1, 2)), equals(2))
  expect_that(length(low(a) * -high(b) * empty()), equals(2))  
  expect_that(length(true(a) & true(b)), equals(2))
})


test_that("Test induced layered pref", {
  
  r <- data.frame(A = c(1, 2, 2), B = c(2, 1, 2), C = c(1, 2, 3))
  
  m <- function(a, r) {
    low(psel(r, a, top = nrow(r), show_level = TRUE)[['.level']])
  }

  m_ <- function(a) {
    low(psel(df__, a, top = nrow(df__), show_level = TRUE)$.level)
  }
  
  a <- m(low(A) * low(B), r) & low(C)
  b <- m_(low(A) * low(B)) & low(C)
  
  expect_that(psel(r, a)$C, equals(1))
  expect_that(psel(r, b)$C, equals(1))
  
  expect_that(pref.str(a, r), matches("low(c(1, 1, 2)) & low(C)", fixed=TRUE))
  expect_that(pref.str(b, r), matches("low(psel(df__, low(A) * low(B), top = nrow(df__), show_level = TRUE)$.level) & low(C)", fixed=TRUE))
})


test_that("Test evaluations", {
  
  p <- low(f(list(a, c(1,2), c(1,b), list(1,c(2,c)))))
  a <- 1
  b <- 1
  df1 <- data.frame(b=NA, c=NA)
  df2 <- data.frame(c=NA)
  
  expect_that(as.character(eval.pref(eval.pref(p, df1), df1)), 
              matches("low(f(list(1, c(1, 2), c(1, b), list(1, c(2, c)))))", fixed=TRUE))
  
  expect_that(as.character(eval.pref(eval.pref(p, df2), df2)), 
              matches("low(f(list(1, c(1, 2), c(1, 1), list(1, c(2, c)))))", fixed=TRUE))
  
  
  g <- function(a, ...) low(f(b, ...) + a + sum(...))
  
  expect_that(as.character(eval.pref(g(1, 2, 3), df1)), 
              matches("low(f(b, 2, 3) + 1 + 5)", fixed = TRUE))
  
  
                
  f <- function(..., x) prod(...) * x
  g <- function(a, ...) low(f(..., x = b) + (a + sum(...)))
  p <- eval.pref(g(1, 2, 3), df1)
  
  expect_that(as.character(p), 
              matches("low(f(2, 3, x = b) + 6)", fixed = TRUE))
  
  expect_that(psel.indices(data.frame(b=c(1,2)), p), equals(1))
  
  p <- eval.pref(eval.pref(p, df2), df2)
  
  expect_that(as.character(p), matches("low(12)", fixed = TRUE))
  
  expect_that(psel.indices(data.frame(b=c(1,2)), p), equals(c(1,2)))
  
  expect_that(as.character(true(b == f(2,3,x=4), df1)), 
              matches("true(b == 24)", fixed = TRUE))
  
  expect_that(as.character(low(f(b, x = 2 * 2), df1)), 
              matches("low(f(b, x = 4))", fixed = TRUE))
})
  