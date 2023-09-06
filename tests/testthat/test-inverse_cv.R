test_that("integral cv is reversible", {

  rand_var <- runif(1, 0.3, 3)
  tolerance <- 1/100

  lnorm_u <- exp(runif(1, -3, 3))
  lnorm_intcv <- dist.intcv("log", v=rand_var, exact=FALSE)
  lnorm_test <- invcv(dist.intcv("log",fun=TRUE), lnorm_intcv)
  expect_lt(abs(lnorm_test/rand_var - 1), tolerance)

  expect_message({invcv(dist.intcv("log",fun=TRUE), lnorm_intcv, verbose=TRUE)})

  logit_u <- runif(1, 0.001, 0.999)
  logit_intcv <- dist.intcv("logit", u=logit_u, v=rand_var)
  logit_test <- invcv(dist.intcv("logit",fun=TRUE), logit_intcv, u=logit_u)
  expect_lt(abs(logit_test/rand_var - 1), tolerance)

})
