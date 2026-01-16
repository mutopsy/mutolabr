test_that("contrast_test_tidy: independent groups (balanced) matches internal 1df ANOVA-style reconstruction", {
  set.seed(101)

  n_per <- 20
  conds <- c("A", "B", "C")
  w <- c(-1, 0, 1)

  dat <- data.frame(
    id   = seq_len(n_per * length(conds)),
    cond = rep(conds, each = n_per),
    y    = rnorm(n_per * length(conds), mean = rep(c(0, 0.2, 0.6), each = n_per), sd = 1),
    w    = rep(w, each = n_per)
  )

  res <- contrast_test_tidy(dat, paired = FALSE, do_round = FALSE, autocorrect_unbalance = TRUE)

  # --- Reconstruct expected F the same way as the function (without calling it) ---
  dc <- dat
  names(dc)[1:4] <- c("s", "x", "y", "c")

  # condition summaries
  ybar <- tapply(dc$y, dc$x, mean)
  nj   <- tapply(dc$y, dc$x, length)
  cj   <- tapply(dc$c, dc$x, function(z) z[1])

  # autocorrect_unbalance applies only when unbalanced; here balanced so no-op
  # center weights at observation level if sum != 0
  # (observation-level mean of c is a weighted mean of cj)
  c_obs_mean <- sum(nj * cj) / sum(nj)
  if (abs(sum(rep(cj, times = nj))) > 1e-5) {
    cj <- cj - c_obs_mean
  }

  # 1df "contrast" component: regress condition means on cj (unweighted across conditions)
  b <- stats::cov(cj, ybar) / stats::var(cj)
  ac_raw <- b * cj
  ac <- ac_raw - mean(ac_raw)  # mean across conditions (unweighted), matching your dc_agg -> mutate(ac=...; ac=ac-mean(ac))

  SS_Ac <- sum(nj * ac^2)

  # Error SS from one-way ANOVA (within-condition residuals)
  fit_x <- stats::lm(y ~ x, data = dc)
  SS_E <- sum(stats::residuals(fit_x)^2)

  df1 <- 1
  df2 <- nrow(dc) - length(unique(dc$x))
  Fexp <- (SS_Ac / df1) / (SS_E / df2)

  testthat::expect_equal(res$df1, df1)
  testthat::expect_equal(res$df2, df2)
  testthat::expect_equal(res$F, Fexp, tolerance = 1e-10)
  testthat::expect_equal(res$p, stats::pf(Fexp, df1, df2, lower.tail = FALSE), tolerance = 1e-12)
})

test_that("contrast_test_tidy: independent groups (unbalanced) matches internal reconstruction with autocorrect", {
  set.seed(102)

  conds <- c("A", "B", "C")
  ns <- c(A = 10, B = 25, C = 15)
  w  <- c(A = -1, B = 0, C = 1)

  dat <- do.call(rbind, lapply(names(ns), function(cc) {
    n <- ns[[cc]]
    data.frame(
      id   = paste0(cc, "_", seq_len(n)),
      cond = cc,
      y    = rnorm(n, mean = c(A = 0, B = 0.1, C = 0.7)[[cc]], sd = 1),
      w    = w[[cc]]
    )
  }))
  rownames(dat) <- NULL

  res <- contrast_test_tidy(dat, paired = FALSE, do_round = FALSE, autocorrect_unbalance = TRUE)

  dc <- dat
  names(dc)[1:4] <- c("s", "x", "y", "c")

  ybar <- tapply(dc$y, dc$x, mean)
  nj   <- tapply(dc$y, dc$x, length)
  cj   <- tapply(dc$c, dc$x, function(z) z[1])

  # autocorrect: convert condition coefficients -> observation coefficients when unbalanced
  if (length(unique(nj)) > 1) {
    cj <- cj / nj
  }

  # observation-level centering rule (weighted by n_j)
  c_obs_mean <- sum(nj * cj) / sum(nj)
  # sum over observations equals sum_j n_j * cj
  if (abs(sum(nj * cj)) > 1e-5) {
    cj <- cj - c_obs_mean
  }

  b <- stats::cov(cj, ybar) / stats::var(cj)
  ac_raw <- b * cj
  ac <- ac_raw - mean(ac_raw)

  SS_Ac <- sum(nj * ac^2)

  fit_x <- stats::lm(y ~ x, data = dc)
  SS_E <- sum(stats::residuals(fit_x)^2)

  df1 <- 1
  df2 <- nrow(dc) - length(unique(dc$x))
  Fexp <- (SS_Ac / df1) / (SS_E / df2)

  testthat::expect_equal(res$df1, df1)
  testthat::expect_equal(res$df2, df2)
  testthat::expect_equal(res$F, Fexp, tolerance = 1e-10)
})

test_that("contrast_test_tidy: autocorrect_unbalance does not necessarily change F (e.g., 2-group unbalanced) but both run", {
  set.seed(103)

  dat <- data.frame(
    id   = 1:(5 + 9),
    cond = c(rep("A", 5), rep("B", 9)),
    y    = rnorm(14),
    w    = c(rep(-1, 5), rep(1, 9))
  )


  r1 <- suppressWarnings(contrast_test_tidy(dat, paired = FALSE, do_round = FALSE, autocorrect_unbalance = TRUE))
  r2 <- suppressWarnings(contrast_test_tidy(dat, paired = FALSE, do_round = FALSE, autocorrect_unbalance = FALSE))

  testthat::expect_true(is.finite(r1$F))
  testthat::expect_true(is.finite(r2$F))
  testthat::expect_equal(r1$df1, 1)
  testthat::expect_equal(r2$df1, 1)
  testthat::expect_equal(r1$df2, nrow(dat) - length(unique(dat$cond)))
  testthat::expect_equal(r2$df2, nrow(dat) - length(unique(dat$cond)))
})

test_that("contrast_test_tidy: paired matches subject-level contrast-score t test in the standard complete case", {
  set.seed(104)

  nsub  <- 30
  conds <- c("A", "B", "C")
  w     <- c(-1, 0, 1)  # sums to 0: no centering needed

  dat <- expand.grid(id = seq_len(nsub), cond = conds, KEEP.OUT.ATTRS = FALSE)
  dat$y <- rnorm(nrow(dat), mean = rep(c(0, 0.2, 0.8), times = nsub), sd = 1)
  dat$w <- w[match(dat$cond, conds)]

  res <- contrast_test_tidy(dat, paired = TRUE, do_round = FALSE, autocorrect_unbalance = TRUE)

  # Subject-level contrast scores: L_i = sum_j c_j y_ij
  Li <- tapply(seq_len(nrow(dat)), dat$id, function(ix) sum(dat$w[ix] * dat$y[ix]))
  Li <- as.numeric(Li)

  tval <- mean(Li) / (stats::sd(Li) / sqrt(nsub))
  Fexp <- tval^2

  testthat::expect_equal(res$df1, 1)
  testthat::expect_equal(res$df2, nsub - 1)
  testthat::expect_equal(res$F, Fexp, tolerance = 1e-10)
  testthat::expect_equal(res$p, stats::pf(Fexp, 1, nsub - 1, lower.tail = FALSE), tolerance = 1e-12)
})

test_that("contrast_test_tidy: input validation", {
  # wrong number of columns
  bad <- data.frame(a = 1:3, b = 1:3, c = 1:3)
  testthat::expect_error(contrast_test_tidy(bad), "exactly four columns")

  # alpha out of bounds
  dat <- data.frame(
    id   = 1:4,
    cond = rep(c("A","B"), each = 2),
    y    = rnorm(4),
    w    = rep(c(-1, 1), each = 2)
  )
  testthat::expect_error(contrast_test_tidy(dat, alpha = 1.2))

})
