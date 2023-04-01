test_that("remove_dic", {

  dat <- remove_dic(ex_itrf)
  dat_2 <- remove_dic(ex_itrf, remove_attributes = TRUE)

  x <- unlist(lapply(dat_2, function(x) attr(x, opt("dic"))))
  expect_equal(any(unlist(lapply(dat, class) == "dic")), FALSE)
  expect_equal(x, NULL)
})
