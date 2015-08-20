

test_that("Test string output of preference", {

  expect_that(empty(), prints_text('[Preference] (empty)', fixed = TRUE))
  
  expect_that(low(a) * low(b), 
              prints_text('[Preference] low(a) * low(b)', fixed = TRUE))
  
  expect_that(low(a) * (low(b) & high(c)), 
              prints_text('[Preference] low(a) * (low(b) & high(c))', fixed = TRUE))
  
  expect_that((low(a) * low(b)) & high(c), 
              prints_text('[Preference] (low(a) * low(b)) & high(c)', fixed = TRUE))
  
})

test_that("Test SQL output", {
  
  expect_that(show.query(empty()), matches(""))
  
  expect_that(show.query(low(a1 * a2) * high(b) * (-high(c) & true(d)), dialect = "EXASOL"),
              matches("PREFERRING LOW (a1 * a2) PLUS HIGH b PLUS (INVERSE (HIGH c) PRIOR TO d)", fixed = TRUE))
  
  expect_that(show.query(low(a1 * a2) * high(b) * (-high(c) & true(d)), dialect = "Preference SQL"),
              matches("PREFERRING (a1 * a2) LOWEST AND b HIGHEST AND ((c HIGHEST) DUAL PRIOR TO d = TRUE)", fixed = TRUE))
  
})


test_that("Test string output of preferences on a given data set", {
  
  f <- function(x) (2*x)
  y <- 1
  
  expect_that(show.pref(low(wt) * low(hp) * low(f(cyl + y)), df = mtcars), 
              prints_text('[Preference] low(wt) * low(hp) * low(f(cyl + 1))', fixed = TRUE))
  
  expect_that(eval.pref(low(wt) * low(hp) * true(f(cyl + y) > y + y), df = mtcars), 
              prints_text('[Preference] low(wt) * low(hp) * true(f(cyl + 1) > 2)', fixed = TRUE))
  
  expect_that(pref.str((low(wt) * low(hp)) & reverse(high(y + f(cyl))), df = mtcars), 
              prints_text('(low(wt) * low(hp)) & -high(1 + f(cyl))', fixed = TRUE))
  
  expect_that(pref.str((low(wt) * low(hp)) & reverse(high(y + f(cyl))), df = mtcars), 
              matches('(low(wt) * low(hp)) & -high(1 + f(cyl))', fixed = TRUE))
  
  expect_that(as.character(eval.pref(-high(f(y) + f(cyl)), df = mtcars)), 
              prints_text('-high(2 + f(cyl))', fixed = TRUE))
  
  expect_that(show.query((low(wt) * low(hp)) & high(cyl + f(wt + y)), df = mtcars),
              matches("PREFERRING (LOW wt PLUS LOW hp) PRIOR TO HIGH (cyl + f(wt + 1))", fixed = TRUE))
  
  expect_that(show.query((low(wt) * low(hp)) | high(cyl + f(wt + y)), df = mtcars, dialect = "PSQL"),
              matches("PREFERRING (wt LOWEST AND hp LOWEST) INTERSECT WITH (cyl + f(wt + 1)) HIGHEST", fixed = TRUE))
})
