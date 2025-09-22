library(dplyr)
library(tidyr)

test_that("independent two-sample (unequal variance, default) runs", {
  set.seed(1)
  dat <- data.frame(
    id   = rep(1:20),
    cond = rep(c("A","B"), times = 10),
    y    = rnorm(20)
  )
  x <- dat[dat$cond=="A","y"]
  y <- dat[dat$cond=="B","y"]

  res1 <- t_test_all_tidy(dat, paired = FALSE, show_table = FALSE)
  res2 <- t_test_all(x,y, paired = FALSE, verbose = FALSE)

  testthat::expect_equal(res1, res2)
})


test_that("paired runs", {
  set.seed(1)
  dat <- data.frame(
    id   = rep(1:10, each = 2),
    cond = rep(c("A","B"), times = 10),
    y    = rnorm(20)
  )
  x <- dat[dat$cond=="A","y"]
  y <- dat[dat$cond=="B","y"]

  res1 <- t_test_all_tidy(dat, paired = TRUE, show_table = FALSE)
  res2 <- t_test_all(x,y, paired = TRUE, verbose = FALSE)

  testthat::expect_equal(res1, res2)
})


test_that("one-sample runs", {
  set.seed(1)
  dat <- data.frame(
    id   = 1:20,
    cond = rep("A", times = 20),
    y    = rnorm(20)
  )
  y <- dat$y

  res1 <- t_test_all_tidy(dat, onesample = TRUE, show_table = FALSE)
  res2 <- t_test_all_tidy(dat[,c("id", "y")], onesample = TRUE, show_table = FALSE)
  res3 <- t_test_all(x = y,y = NULL, verbose = FALSE)

  testthat::expect_equal(res1, res2)
  testthat::expect_equal(res1, res3)
})
