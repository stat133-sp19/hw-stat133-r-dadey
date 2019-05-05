
test_that(
  "mean auxillary is valid",
          {expect_true(aux_mean(-1, .5) == -0.5)
          expect_error(aux_mean(1))
          expect_true(aux_mean(1.5, .5) == 0.75)
          expect_true(aux_mean(1, 1.5) == 1.5)
          expect_true(aux_mean(1, .5) == 0.5)}
          )


test_that(
  "variance auxillary is valid",
  {expect_true(aux_variance(-1, .5) == -0.25)
  expect_error(aux_variance(1))
  expect_true(aux_variance(1.5, .5) == 0.375)
  expect_true(aux_variance(1, 1.5) == -0.75)
  expect_true(aux_variance(1, .5) == 0.25)}
)

#Define vectors for testing
a <- c(2, .5) #np + p not integer
b <- c(1, .5) #np + p integer

test_that(
  "mode auxillary is valid",
  {expect_true(length(aux_mode(a[1], a[2])) == 1)
  expect_true(length(aux_mode(b[1], b[2])) == 2)
  expect_error(aux_mode(1))
  expect_true(aux_mode(-2,.5) == -1)
  expect_true(aux_mode(0.5,1) == 1)
})


test_that(
  "skewness auxillary is valid",
  {expect_true(aux_skewness(10,1) == -Inf)
  expect_true(aux_skewness(10,0) == Inf)
  expect_error(aux_skewness(1))
})

test_that(
  "kurtosis auxillary is valid",
  {expect_true(aux_kurtosis(10,1) == Inf)
  expect_true(aux_kurtosis(10,0) == Inf)
  expect_error(aux_kurtosis(1))
})



