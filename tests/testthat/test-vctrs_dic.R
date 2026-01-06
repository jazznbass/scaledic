# tests/testthat/test-vctrs_dic.R

testthat::test_that("base c() keeps dic metadata when first arg is dic", {
  x <- new_dic(1:3, item_name = "it1", values = "1:3")
  y <- c(x, 4L)

  testthat::expect_s3_class(y, "dic")
  testthat::expect_identical(dic_attr(y, "item_name"), dic_attr(x, "item_name"))
  testthat::expect_identical(attr(y, opt("dic")), attr(x, opt("dic")))
  testthat::expect_identical(attr(y, "label"), attr(x, "label"))
  testthat::expect_identical(attr(y, "labels"), attr(x, "labels"))
  testthat::expect_equal(length(y), 4)
})

testthat::test_that("vctrs::vec_c keeps dic metadata when combining base + dic (both orders)", {
  testthat::skip_if_not_installed("vctrs")

  x <- new_dic(1:3, item_name = "it1", values = "1:3")

  y1 <- vctrs::vec_c(x, 4L)
  testthat::expect_s3_class(y1, "dic")
  testthat::expect_identical(attr(y1, opt("dic")), attr(x, opt("dic")))

  y2 <- vctrs::vec_c(0L, x)
  testthat::expect_s3_class(y2, "dic")
  testthat::expect_identical(attr(y2, opt("dic")), attr(x, opt("dic")))
})

testthat::test_that("vctrs::vec_c preserves dic for dic + dic (left template wins)", {
  testthat::skip_if_not_installed("vctrs")

  x <- new_dic(1:2, item_name = "it_left", values = "1:2")
  y <- new_dic(3:4, item_name = "it_right", values = "1:4")

  z <- vctrs::vec_c(x, y)

  testthat::expect_s3_class(z, "dic")
  testthat::expect_identical(dic_attr(z, "item_name"), dic_attr(x, "item_name"))
  testthat::expect_identical(attr(z, opt("dic")), attr(x, opt("dic")))
})

testthat::test_that("vec_c works for dic with integer/double/logical/character/factor inputs", {
  testthat::skip_if_not_installed("vctrs")

  # integer dic
  xi <- new_dic(1:3, item_name = "i", values = "1:3")
  zi <- vctrs::vec_c(xi, 4L, 5L)
  testthat::expect_s3_class(zi, "dic")
  testthat::expect_identical(attr(zi, opt("dic")), attr(xi, opt("dic")))

  # double dic
  xd <- new_dic(as.double(1:3), item_name = "d", type = "double", values = "1, 3")
  zd <- vctrs::vec_c(0.5, xd, 4.5)
  testthat::expect_s3_class(zd, "dic")
  testthat::expect_identical(attr(zd, opt("dic")), attr(xd, opt("dic")))

  # logical dic
  xl <- new_dic(c(TRUE, FALSE), item_name = "l", type = "logical", values = "TRUE, FALSE")
  zl <- vctrs::vec_c(xl, TRUE, FALSE)
  testthat::expect_s3_class(zl, "dic")
  testthat::expect_identical(attr(zl, opt("dic")), attr(xl, opt("dic")))

  # character dic
  xc <- new_dic(c("a", "b"), item_name = "c", type = "character", values = "'a','b'")
  zc <- vctrs::vec_c("z", xc, "y")
  testthat::expect_s3_class(zc, "dic")
  testthat::expect_identical(attr(zc, opt("dic")), attr(xc, opt("dic")))

  # factor dic
  xf <- new_dic(factor(c("m", "f"), levels = c("m", "f")),
                item_name = "f", type = "factor", values = "'m','f'")
  zf <- vctrs::vec_c(xf, factor("m", levels = c("m", "f")))
  testthat::expect_s3_class(zf, "dic")
  testthat::expect_identical(attr(zf, opt("dic")), attr(xf, opt("dic")))
})

testthat::test_that("vec_cast restores dic metadata when casting base to dic prototype", {
  testthat::skip_if_not_installed("vctrs")

  x <- new_dic(1:3, item_name = "it1", values = "1:3")

  out <- vctrs::vec_cast(1:2, x)
  testthat::expect_s3_class(out, "dic")
  testthat::expect_identical(attr(out, opt("dic")), attr(x, opt("dic")))
})

testthat::test_that("subsetting preserves dic metadata after vec_c results", {
  testthat::skip_if_not_installed("vctrs")

  x <- new_dic(1:3, item_name = "it1", values = "1:3")
  y <- vctrs::vec_c(x, 4L, 5L)
  y2 <- y[1:3]

  testthat::expect_s3_class(y2, "dic")
  testthat::expect_identical(attr(y2, opt("dic")), attr(x, opt("dic")))
  testthat::expect_equal(length(y2), 3)
})
