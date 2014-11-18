

test_that("Test string output of preference", {

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
