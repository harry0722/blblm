test_that("blblm works", {
  library(nycflights13)
  library(stringr)
  set.seed(141)
  m <-9
  groups <- sample(seq_len(m), nrow(flights), replace = TRUE)
  flights$group=groups
  dir.create("flights/", showWarnings = FALSE)
  for (i in seq_len(m)) {
    write_csv(flights[which(flights$group==i),], str_c("flights/", i, ".csv"))
  }
  fit <- blblm(distance~air_time, data = flights, m = 9, B = 100)
  expect_s3_class(fit, "blblm")
  coe1<-coef(fit)
  expect_equal(length(coe1),c(n,m))
})
