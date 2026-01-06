# tests/testthat/test-extract_values.R

testthat::test_that(".extract_values parses valid specs and rejects invalid ones", {

  # Helper to shorten calls
  ev <- function(values, type, field = "values", item = "it1") {
    scaledic:::.extract_values(values = values, type = type, item = item, field = field)
  }

  # ----- NULL / empty -----
  testthat::expect_null(ev(NULL, "integer"))
  testthat::expect_null(ev(NA, "integer"))
  testthat::expect_null(ev("", "integer"))
  testthat::expect_null(ev("   ", "integer"))

  # ----- integer: comma list -----
  testthat::expect_equal(ev("1, 2, 3, 4", "integer"), c(1, 2, 3, 4))

  # ----- integer: ranges (ascending / descending) -----
  testthat::expect_equal(ev("5:7", "integer"), c(5, 6, 7))
  testthat::expect_equal(ev("7:5", "integer"), c(7, 6, 5))

  # ----- integer: mixed list + range + duplicates (unique at end) -----
  testthat::expect_equal(ev("1, 3:5, 5, 1", "integer"), c(1, 3, 4, 5))

  # ----- numeric: decimals allowed -----
  testthat::expect_equal(ev("1.5, 2, 3.25", "numeric"), c(1.5, 2, 3.25))

  # ----- numeric: invalid token -> NULL -----
  testthat::expect_null(ev("1, a, 3", "numeric"))

  # ----- integer: non-integer token -> returns unique values but message is emitted -----
  testthat::expect_silent({
    out <- ev("1, 2.5, 3", "integer")
    testthat::expect_true(is.numeric(out))
    testthat::expect_true(all(out %in% c(1, 2.5, 3)))
  })

  # ----- invalid range tokens -> NULL -----
  testthat::expect_null(ev("1:2:3", "integer"))
  testthat::expect_null(ev("a:3", "integer"))
  testthat::expect_null(ev("1:b", "integer"))
  testthat::expect_null(ev("1.2:3", "integer")) # endpoints must be integers
  testthat::expect_null(ev("1:3.5", "integer"))

  # ----- float/double values field: must be exactly two numbers and ordered -----
  testthat::expect_equal(ev("5, 11", "double", field = "values"), c(5, 11))
  testthat::expect_null(ev("5", "double", field = "values"))
  testthat::expect_null(ev("5, 11, 12", "double", field = "values"))
  testthat::expect_null(ev("11, 5", "double", field = "values"))

  # Also applies to type "double" in your implementation (field == "values")
  testthat::expect_equal(ev("0, 1", "double", field = "values"), c(0, 1))
  testthat::expect_null(ev("0, 1, 2", "double", field = "values"))

  # ----- missing field: for float/double you do NOT enforce length==2 -----
  testthat::expect_equal(ev("5, 11, 12", "double", field = "missing"), c(5, 11, 12))
  testthat::expect_equal(ev("0:2", "double", field = "missing"), c(0, 1, 2))

  # ----- character/factor: quoted values are unquoted -----
  testthat::expect_equal(ev("'m','f','d'", "character"), c("m", "f", "d"))
  testthat::expect_equal(ev("\"m\", \"f\"", "factor"), c("m", "f"))

  # ----- character/factor: unquoted values are auto-quoted (your behavior) -----
  testthat::expect_equal(ev("m, f", "character"), c("'m'", "'f'"))

  # ----- character/factor: embedded same-quote -> returns NULL via NA check -----
  testthat::expect_null(ev("'m'k'", "character"))
  testthat::expect_null(ev("\"m\"k\"", "factor"))

  # ----- numerics reject quotes/letters in token -> NULL -----
  testthat::expect_null(ev("'1', 2", "numeric"))
  testthat::expect_null(ev("\"1\", 2", "numeric"))
  testthat::expect_null(ev("system(1), 2", "numeric"))
})


