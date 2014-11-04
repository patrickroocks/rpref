

test_that("Test Hasse function from C", {

  pref <- low(mpg) * low(hp)
  expect_that(get_hasse_impl(pref$get_scorevals(1, mtcars[1:10,])$scores, pref$serialize()), 
              equals(matrix(c(2,8,5,0,5,1,5,4,5,9,0,3,1,3),2)))
  
  pref <- pos(cyl, 2) * low(hp+mpg)
  expect_that(get_hasse_impl(pref$get_scorevals(1, mtcars[1:10,])$scores, pref$serialize()), 
              equals(matrix(c(7,2,2,8,8,5,5,0,5,1,0,3,1,3,3,9,9,4,4,6),2)))
  
})


library(igraph)

test_that("Test igraph output for mtcars[1:5,] with low(mpg)", {
  
  g <- get_btg(mtcars[1:5,], low(mpg))$graph
  
  expect_that(as.numeric(g['1']), equals(c(0,0,0,1,0)))
  expect_that(as.numeric(g['2']), equals(c(0,0,0,1,0)))
  expect_that(as.numeric(g['3']), equals(c(0,0,0,0,0)))
  expect_that(as.numeric(g['4']), equals(c(0,0,1,0,0)))
  expect_that(as.numeric(g['5']), equals(c(1,1,0,0,0)))
  
  expect_that(g['4','3'], equals(1))
  expect_that(g['3','4'], equals(0))
  expect_that(g['2','4'], equals(1))
  expect_that(g['4','2'], equals(0))
  expect_that(g['5','2'], equals(1))
  expect_that(g['2','5'], equals(0))
  expect_that(g['5','4'], equals(0))
  expect_that(g['4','5'], equals(0))
})
