test_that("apply_extract_dic", {

  dat_1 <- suppressMessages(apply_dic(dat_itrf, dic_itrf))
  dat_2 <- suppressMessages(apply_dic(dat_itrf, extract_dic(dat_1)))

  expect_equal(dat_1, dat_2)
})
test_that("apply_dic with missing items in data", {

  dic_mod <- dic_itrf[-c(1,3,5), ]
  dat_mod <- dat_itrf[, -c(1,3,5)]

  dat_1 <- suppressMessages(apply_dic(dat_mod, dic_mod))
  dat_2 <- suppressMessages(apply_dic(dat_mod, extract_dic(dat_1)))

  expect_equal(dat_1, dat_2)
})
test_that("apply_dic with extra items in data", {

  dic_mod <- dic_itrf
  dat_mod <- dat_itrf
  dat_mod$extra_item_1 <- sample(1:5, nrow(dat_mod), replace = TRUE)
  dat_mod$extra_item_2 <- sample(1:5, nrow(dat_mod), replace = TRUE)

  dat_1 <- suppressMessages(apply_dic(dat_mod, dic_mod))
  dat_2 <- suppressMessages(apply_dic(dat_mod, extract_dic(dat_1)))

  expect_equal(dat_1, dat_2)
})
