

test_that("Test computation of Hasse diagramm", {

  expect_equal(get_hasse_diag(mtcars[1:10,], low(mpg) * low(hp)), 
              t(matrix(c(1,4,2,4,3,9,6,1,6,2,6,5,6,10),2)))
  
  expect_equal(get_hasse_diag(mtcars[1:10,], pos(cyl, 2) * low(hp + mpg)), 
              t(matrix(c(1,4,2,4,3,9,4,10,5,7,6,1,6,2,8,3,9,6,10,5),2)))
  
  df <- data.frame(id = 1:4)
  expect_equal(get_hasse_diag(df, (true(id == 1) | -true(id == 2)) + (true(id == 3) | -true(id == 4)) ), 
              t(matrix(c(1, 2, 3, 4), 2)))
  
  expect_equal(get_hasse_diag(df, layered(id, 1, 2, 3, 4)), 
              t(matrix(c(1, 2, 2, 3, 3, 4), 2)))
})


test_that("Test predecessors and successors", {
  
  # ** Generate preference,  init succ/pred functions and do some test
  df <- data.frame(id = 1:5)
  pref <- ((true(id %in% c(1,2)) & true(id == 3)) * true(id == 4)) & true(id == 5)
  init_pred_succ(pref, df)
  
  expect_equal(all_succ(pref, numeric(0)), numeric(0))
  expect_equal(all_succ(pref, 1), c(3,5))
  expect_equal(all_succ(pref, 4), 5)
  expect_equal(all_succ(pref, c(2,4)), c(3,5))
  
  expect_equal(all_pred(pref, c(2,4)), numeric(0))
  expect_equal(all_pred(pref, 5), c(1,2,3,4))
  
  expect_equal(hasse_succ(pref, numeric(0)), numeric(0))
  expect_equal(hasse_succ(pref, c(1,2)), 3)
  expect_equal(hasse_succ(pref, c(2,4)), c(3,5))
  expect_equal(hasse_succ(pref, c(4,2)), c(3,5))
  expect_equal(hasse_succ(pref, 5), numeric(0))
  
  expect_equal(hasse_pred(pref, 5), c(3,4))
  expect_equal(hasse_pred(pref, c(3,4)), c(1,2))
  
  # Another test case
  p <- (((true(id == 1, df) * true(id == 2)) & true(id == 3)) * (true(id == 2) & true(id == 4))) & true(id == 5)
  init_pred_succ(p)
  
  expect_equal(hasse_pred(p, c(3,4)), c(1,2))
  expect_equal(hasse_pred(p, c(3,4), intersect = TRUE), 2)
  
  expect_equal(hasse_succ(p, c(1,2)), c(3,4))
  expect_equal(hasse_succ(p, c(1,2), intersect = TRUE), 3)

  expect_equal(all_pred(p, c(3,5)), c(1,2,3,4))
  expect_equal(all_pred(p, c(3,5), intersect = TRUE), c(1,2))

  expect_equal(all_succ(p, c(1,4)), c(3,5))
  expect_equal(all_succ(p, c(1,4), intersect = TRUE), 5)
  
  # A test case with eval and reverse
  df <- data.frame(id = 1:5)
  a <- 1
  b <- 2
  c <- 3
  p <- ((true(id == a) * true(id == b)) & true(id == c)) * (true(id == b) & -true(id %in% c(a,b,c)) & -true(id == 5))
  
  expect_error(all_succ(p, 1)) # Need to call init_pred_succ first!
  
  # Evaluate pref (to substitute a,b,c in true(id %in% c(a,b,c))) and associate data.frame
  assoc.df(p) <- df
  
  expect_identical(as.character(p), 
                   '((true(id == 1) * true(id == 2)) & true(id == 3)) * (true(id == 2) & -true(id %in% c(1, 2, 3)) & -true(id == 5))')
  
  # We have to init p after evaluation! (but df is already associcated!)
  init_pred_succ(p)
  expect_equal(all_succ(p, 1), 3)
  expect_equal(all_pred(p, 5), c(2,4))
  expect_equal(hasse_succ(p, 2), c(3,4))
  expect_equal(all_succ(p, c(1,2), intersect = TRUE), 3)
  expect_equal(all_pred(p, c(3,4), intersect = TRUE), 2)
})


# ---------------------------------------------------------------------------


# should work without attaching igraph package (no plotting)
test_that("Test igraph output for mtcars[1:5,] with low(mpg)", {
  
  # Trivial tests
  expect_error(get_btg(mtcars[NULL,], empty(), use_dot = FALSE))
  
  g <- get_btg(mtcars[1:5,], empty(), use_dot = FALSE)$graph
  expect_equal(as.numeric(g['1']), c(0,0,0,0,0))
  
  # Main test
  g <- get_btg(mtcars[1:5,], low(mpg), use_dot = FALSE)$graph
  
  expect_equal(as.numeric(g['1']), c(0,0,0,1,0))
  expect_equal(as.numeric(g['2']), c(0,0,0,1,0))
  expect_equal(as.numeric(g['3']), c(0,0,0,0,0))
  expect_equal(as.numeric(g['4']), c(0,0,1,0,0))
  expect_equal(as.numeric(g['5']), c(1,1,0,0,0))
  
  expect_equal(g['4','3'], 1)
  expect_equal(g['3','4'], 0)
  expect_equal(g['2','4'], 1)
  expect_equal(g['4','2'], 0)
  expect_equal(g['5','2'], 1)
  expect_equal(g['2','5'], 0)
  expect_equal(g['5','4'], 0)
  expect_equal(g['4','5'], 0)
})

if ("Rgraphviz" %in% rownames(installed.packages())) {
  
  # should work without attaching Rgraphviz package (no plotting)
  test_that("Test igraph output for mtcars[1:5,] with low(wt)", {
  
    # Trivial test
    g <- get_btg(mtcars[1:5,], empty(), use_dot = TRUE)
    expect_equal(g@edgeL, list('1' = NULL, '2' = NULL, '3' = NULL, '4' = NULL, '5' = NULL))
  
    # Main test
    g <- get_btg(mtcars[1:5,], low(mpg), use_dot = TRUE)
    expect_equal(g@edgeL, list('1' = list(edges = 4), '2' = list(edges = 4), '3' = NULL, 
                               '4' = list(edges = 3), '5' = list(edges = c(1, 2))))
    
  })
  
}


# ---------------------------------------------------------------------------


test_that("Test dot string output for mtcars[1:5,] with simple preferences", {
  
  expect_error(get_btg_dot(mtcars[NULL,], empty()))
  
  # Warning because of identical labels
  expect_warning(get_btg_dot(mtcars[1:5,], empty(), label = mtcars[1:5,]$hp))
  
  # Tirvial test, empty preference
  expect_equal(get_btg_dot(mtcars[1:5,], empty(), label = rownames(mtcars[1:5,])),
               paste0("digraph G {\n{\nrank=same;\n1;\n2;\n3;\n4;\n5;\n}\n",
                      "\"1\" [label=\"Mazda RX4\"]\n\"2\" [label=\"Mazda RX4 Wag\"]\n\"3\" [label=\"Datsun 710\"]\n",
                      "\"4\" [label=\"Hornet 4 Drive\"]\n\"5\" [label=\"Hornet Sportabout\"]\n}"))
  
  # high(hp) preference
  expect_equal(get_btg_dot(mtcars[1:5,], high(hp), label = rownames(mtcars[1:5,])),
               paste0("digraph G {\n{\nrank=same;\n5;\n}\n{\nrank=same;\n1;\n2;\n4;\n}\n{\nrank=same;\n3;\n}\n",
                      "\"1\" [label=\"Mazda RX4\"]\n\"2\" [label=\"Mazda RX4 Wag\"]\n",
                      "\"3\" [label=\"Datsun 710\"]\n\"4\" [label=\"Hornet 4 Drive\"]\n\"5\" [label=\"Hornet Sportabout\"]\n",
                      "1 -> 3;\n2 -> 3;\n4 -> 3;\n5 -> 1;\n5 -> 2;\n5 -> 4;\n}"))
  
})