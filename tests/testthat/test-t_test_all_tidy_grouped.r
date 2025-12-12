library(dplyr)
library(tidyr)

test_that("independent two-sample (unequal variance, default) runs", {
  dat <- data_fict_between
  dat_con <- dat %>% filter(group == "control") %>% dplyr::select(-group)
  dat_int <- dat %>% filter(group == "intervention") %>% dplyr::select(-group)

  res1 <- t_test_all_tidy_grouped(dat, paired = FALSE, show_design = FALSE)
  res2_1 <- t_test_all_tidy(dat_con, paired = FALSE, show_design = FALSE)
  res2_2 <- t_test_all_tidy(dat_int, paired = FALSE, show_design = FALSE)

  testthat::expect_equal(
    res1 %>% slice(1) %>% dplyr::select(-group),
    res2_1
    )

  testthat::expect_equal(
    res1 %>% slice(2) %>% dplyr::select(-group),
    res2_2
  )
})

test_that("paired runs", {
  dat <- data_fict_mixed
  dat_man <- dat %>% filter(gender == "man") %>% dplyr::select(-gender)
  dat_wom <- dat %>% filter(gender == "woman") %>% dplyr::select(-gender)

  res1 <- t_test_all_tidy_grouped(dat, paired = TRUE, show_design = FALSE)
  res2_1 <- t_test_all_tidy(dat_man, paired = TRUE, show_design = FALSE)
  res2_2 <- t_test_all_tidy(dat_wom, paired = TRUE, show_design = FALSE)

  testthat::expect_equal(
    res1 %>% slice(1) %>% dplyr::select(-gender),
    res2_1
  )

  testthat::expect_equal(
    res1 %>% slice(2) %>% dplyr::select(-gender),
    res2_2
  )
})


test_that("one-sample runs", {
  dat <- data_fict_between %>%
    dplyr::select(-group)
  dat_man <- dat %>% filter(gender == "man") %>% dplyr::select(-gender)
  dat_wom <- dat %>% filter(gender == "woman") %>% dplyr::select(-gender)

  res1 <- t_test_all_tidy_grouped(dat, onesample = TRUE, mu = 30, show_design = FALSE)
  res2_1 <- t_test_all_tidy(dat_man, onesample = TRUE, mu = 30, show_design = FALSE)
  res2_2 <- t_test_all_tidy(dat_wom, onesample = TRUE, mu = 30, show_design = FALSE)

  testthat::expect_equal(
    res1 %>% slice(1) %>% dplyr::select(-gender, -mean),
    res2_1
  )

  testthat::expect_equal(
    res1 %>% slice(2) %>% dplyr::select(-gender, -mean),
    res2_2
  )
})
