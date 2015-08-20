

test_that("Test computation of Hasse diagramm", {

  expect_that(get_hasse_diag(mtcars[1:10,], low(mpg) * low(hp)), 
              equals(t(matrix(c(1,4,2,4,3,9,6,1,6,2,6,5,6,10),2))))
  
  expect_that(get_hasse_diag(mtcars[1:10,], pos(cyl, 2) * low(hp + mpg)), 
              equals(t(matrix(c(1,4,2,4,3,9,4,10,5,7,6,1,6,2,8,3,9,6,10,5),2))))
  
  df <- data.frame(id = 1:4)
  expect_that(get_hasse_diag(df, (true(id == 1) | -true(id == 2)) + (true(id == 3) | -true(id == 4)) ), 
              equals(t(matrix(c(1, 2, 3, 4),2))))
})


test_that("Test predecessors and successors", {
  
  # ** Generate preference,  init succ/pred functions and do some test
  df <- data.frame(id = 1:5)
  pref <- ((true(id %in% c(1,2)) & true(id == 3)) * true(id == 4)) & true(id == 5)
  init_pred_succ(df, pref)
  
  expect_that(all_succ(pref, numeric(0)), equals(numeric(0)))
  expect_that(all_succ(pref, 1), equals(c(3,5)))
  expect_that(all_succ(pref, 4), equals(5))
  expect_that(all_succ(pref, c(2,4)), equals(c(3,5)))
  
  expect_that(all_pred(pref, c(2,4)), equals(numeric(0)))
  expect_that(all_pred(pref, 5), equals(c(1,2,3,4)))
  
  expect_that(hasse_succ(pref, numeric(0)), equals(numeric(0)))
  expect_that(hasse_succ(pref, c(1,2)), equals(3))
  expect_that(hasse_succ(pref, c(2,4)), equals(c(3,5)))
  expect_that(hasse_succ(pref, c(4,2)), equals(c(3,5)))
  expect_that(hasse_succ(pref, 5), equals(numeric(0)))
  
  expect_that(hasse_pred(pref, 5), equals(c(3,4)))
  expect_that(hasse_pred(pref, c(3,4)), equals(c(1,2)))
  
  # Another test case
  p <- (((true(id == 1) * true(id == 2)) & true(id == 3)) * (true(id == 2) & true(id == 4))) & true(id == 5)
  init_pred_succ(df, p)
  
  expect_that(hasse_pred(p, c(3,4)), equals(c(1,2)))
  expect_that(hasse_pred(p, c(3,4), intersect = TRUE), equals(2))
  
  expect_that(hasse_succ(p, c(1,2)), equals(c(3,4)))
  expect_that(hasse_succ(p, c(1,2), intersect = TRUE), equals(3))

  expect_that(all_pred(p, c(3,5)), equals(c(1,2,3,4)))
  expect_that(all_pred(p, c(3,5), intersect = TRUE), equals(c(1,2)))

  expect_that(all_succ(p, c(1,4)), equals(c(3,5)))
  expect_that(all_succ(p, c(1,4), intersect = TRUE), equals(5))
  
  # A test case with eval and reverse
  df <- data.frame(id = 1:5)
  a <- 1
  b <- 2
  c <- 3
  p <- ((true(id == a) * true(id == b)) & true(id == c)) * (true(id == b) & -true(id %in% c(a,b,c)) & -true(id == 5))
  
  expect_that(all_succ(p, 1), throws_error()) # Need to call init_pred_succ first!
  
  # Evaluate pref (to substitute a,b,c in true(id %in% c(a,b,c)))
  p <- eval.pref(p, df)
  
  expect_that(as.character(p), 
              matches('((true(id == 1) * true(id == 2)) & true(id == 3)) * (true(id == 2) & -true(id %in% c(1, 2, 3)) & -true(id == 5))', fixed = TRUE))
  
  # We have to init p after evaluation!
  init_pred_succ(df, p)
  
  expect_that(all_succ(p, 1), equals(3))
  expect_that(all_pred(p, 5), equals(c(2,4)))
  expect_that(hasse_succ(p, 2), equals(c(3,4)))
  expect_that(all_succ(p, c(1,2), intersect = TRUE), equals(3))
  expect_that(all_pred(p, c(3,4), intersect = TRUE), equals(2))  
})


# ---------------------------------------------------------------------------


library(igraph)

test_that("Test igraph output for mtcars[1:5,] with low(mpg)", {
  
  g <- get_btg(mtcars[1:5,], low(mpg))$graph
  
  plot_btg(mtcars[1:5,], low(mpg))
  
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
