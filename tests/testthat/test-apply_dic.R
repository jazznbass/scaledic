test_that("apply_extract_dic", {

  dat_1 <- suppressMessages(apply_dic(dat_itrf, dic_itrf))
  dat_2 <- suppressMessages(apply_dic(dat_itrf, extract_dic(dat_1)))

  expect_equal(dat_1, dat_2)
})
