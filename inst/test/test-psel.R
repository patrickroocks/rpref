
library(dplyr)

# Run all tests for parallel AND for non-parallel mode
for (parallelity in c(FALSE, TRUE)) {
  
  options(rPref.parallel = parallelity)
  
  # Simple tests 
  test_that("Test Preference selection on simple test sets", {
    expect_that(psel(data.frame(a = c(3,2,1,1,4)), low(a))$a, equals(c(1,1)))
    expect_that(psel.indices(data.frame(a = c(3,3,2,1,1,4)), low(a)), equals(c(4,5)))
  })
  
  # Empty preference
  test_that("Test empty preference", {
    expect_that(psel(data.frame(a = c(3,2,1,1,4)), empty() & high(a))$a, equals(4))
    expect_that(psel(data.frame(a = c(3,2,1,1,4)), empty())$a, equals(c(3,2,1,1,4)))
    expect_that(psel(mtcars, low(mpg) & (empty() * low(hp)))$hp, equals(205))
  })
  
  # Empty dataset
  test_that("Test empty dataset", {
    expect_that(psel(data.frame(a = 1)[NULL,,drop=FALSE], low(a)), equals(data.frame(a = 1)[NULL,,drop=FALSE]))
  })  

  # More tests for psel/psel.indices and the preference constructors
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  test_that("Test Preference selection", {
    expect_that(psel(mtcars, low(mpg))$mpg, equals(c(10.4, 10.4)))
    expect_that(psel(mtcars, low(mpg) & low(hp))$hp, equals(205))
    expect_that(sort(psel(mtcars, high(mpg) * high(hp))$mpg), equals(c(15, 15.8, 17.3, 19.7, 30.4, 32.4, 33.9)))
    
    expect_that(sort(psel(mtcars, true(mpg < 15) * true(am == 0))$mpg), equals(c(10.4, 10.4, 13.3, 14.3, 14.7)))
  })
  
  
  test_that("Test Preference selection with indices", {
    expect_that(sort(psel.indices(mtcars, high(am) * true(vs == 1))), equals(c(3, 18, 19, 20, 26, 28, 32)))
  })

  
  # Note that dplyr also uses "between" since v0.3
  test_that("Test base preference macros and prior chains", {
    expect_that(psel(mtcars, pos(carb, 2))$carb, equals(rep(2, 10)))
    expect_that(psel(mtcars, around(mpg, 20))$mpg, equals(19.7))
    expect_that(psel(mtcars, rPref::between(hp, 110, 120))$hp, equals(c(110, 110, 110, 113)))
    expect_that(psel(mtcars, rPref::between(hp, 115, 122))$hp, equals(c(123, 123)))
    
    expect_that(psel(mtcars, -layered(cyl, c(4, 6), 8))$cyl, equals(rep(8, 14)))
    expect_that(rownames(psel(mtcars, true(mpg < 22) & true(cyl == 4) & true(wt < 3 & gear == 4))), equals("Volvo 142E"))
    
  })
  
  test_that("Test if environments are found correctly", {
    test_fun <- function() {
      f <- function(x) -x
      return(low(f(mpg)))
    }
    expect_that(psel(mtcars, test_fun()), equals(psel(mtcars, -low(mpg))))
  })
  
  
  test_that("Behavior of group_by function from dplyr package", {
    expect_that(attr(group_by(mtcars[1:5,], cyl), 'indices'), equals(list(2, c(0,1,3), 4)))
  })
  
  
  test_that("Grouped preference selection", {
    expect_that(psel(group_by(mtcars, cyl), low(mpg))$mpg, equals(c(21.4, 17.8, 10.4, 10.4)))
    expect_that(as.data.frame(summarise(psel(group_by(mtcars, cyl), low(mpg) * low(hp)), n())), equals(data.frame(cyl=c(4,6,8),'n()'=c(5,2,2), check.names=FALSE)))
    expect_that(psel(group_by(mtcars, cc = cyl * carb), true(hp==110) & low(hp))$cc, equals(c(4, 6, 8, 16, 16, 24, 24, 32, 36, 64)))
  })
  
  
  # Simple tests of top-K, at_least and toplevel
  test_that("Test TOP-k Preference selection", {
    df <- data.frame(a = c(3,2,1,1,4), b = c(1,1,1,2,2)) # Simple data set
    
    # Check correct indices and level values
    expect_that(sort(psel.indices(df, low(a), top=5)), equals(1:5))
    expect_that(psel(df, low(a), at_least = 2), equals(data.frame(c(1,1), c(1, 2), c(1,1)), check.attributes = FALSE))
    expect_that(psel.indices(df, low(a), at_least = 3, top = 2), equals(c(3,4)))
    expect_that(psel(df, low(a), top_level = 2)$b, equals(c(1,2,1)))
    expect_that(psel.indices(df, high(a), at_least = 2, top_level = 3, show_level = TRUE)$.indices, equals(c(5,1)))
    expect_that(psel(df, high(a), at_least = 2, top_level = 2, and_connected = FALSE)$.level, equals(c(1,2)))
    expect_that(psel(df, around(a,2), at_least = 2, top = 3, top_level = 2, and_connected = FALSE)$.level, equals(c(1,2,2,2)))
    expect_that(psel(df, around(a,2), at_least = 10)$a, equals(c(2,3,1,1,4)))
    expect_that(psel(df, around(a,2), top_level = 1)$.level, equals(1))
    expect_that(psel(df, low(a+b), at_least = 5)$.level, equals(c(1,2,2,3,4)))
    
    # Check if show_level works correctly
    expect_that(psel(df, low(a), show_level = TRUE)$.level, equals(c(1,1)))
    expect_that(ncol(psel(df, low(a))), equals(2))
    expect_that(ncol(psel(df, low(a), show_level = TRUE)), equals(3))
    expect_that(length(psel.indices(df, low(a))), equals(2)) # ncol is NULL
    expect_that(length(psel.indices(df, low(a), top_level = 1)), equals(2))
    expect_that(ncol(psel.indices(df, low(a), show_level = TRUE)), equals(2))
    expect_that(ncol(psel(df, low(a), top = 1)), equals(3))
    
  })
  
  # Simple tests of grouped top-K, at_least and toplevel
  test_that("Test TOP-k grouped Preference selection", {
    dfg <- group_by(data.frame(a = c(3,2,1,1,4), b = c(1,1,1,2,2)), b) # Simple grouped dataset
    expect_that(as.data.frame(psel(dfg, low(a), top = 2)), equals(data.frame(c(1,2,1,4), c(1,1,2,2), c(1,2,1,2)), check.attributes = FALSE))
    expect_that(psel(dfg, low(a), at_least = 2, top = 1)$a, equals(c(1,1)))
    expect_that(psel(dfg, high(a), top_level = 2)$.level, equals(c(1,2,1,2)))
    expect_that(psel.indices(dfg, around(a,2), at_least = 1, top_level = 2), equals(c(2,4)))
    expect_that(psel.indices(dfg, low(b) * high(a), at_least = 2, top_level = 2, show_level = TRUE)$.level, equals(c(1,2,1,2)))
  })

  # Top-K Tests on mtcars
  test_that("Test TOP-k Preference selection on mtcars", {
    expect_that(psel.indices(mtcars, low(mpg + hp), top = 5), equals(order(mtcars$mpg + mtcars$hp)[1:5]))
    expect_that(sort(psel(mtcars, layered(cyl, c(4, 6), 8), top = 4)$cyl), equals(c(4, 6, 6, 6)))
    expect_that(psel(mtcars, low(mpg), top = 5)$mpg, equals(c(10.4, 10.4, 13.3, 14.3, 14.7)))
    expect_that(psel(group_by(mtcars, cyl), low(mpg), top = 3)$mpg, equals(c(21.4, 21.5, 22.8, 17.8, 18.1, 19.2, 10.4, 10.4, 13.3)))
    expect_that(psel(group_by(mtcars, cyl), low(mpg) * high(hp), at_least = 3)$.level, equals(c(1,1,2,1,1,2,2,1,1,1)))
  })
  
}  
