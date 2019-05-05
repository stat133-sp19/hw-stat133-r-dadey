test_that("choose function is valid",
  {
  expect_error(bin_choose(1))
  expect_error(bin_choose(1,2))
  expect_true(bin_choose(5, 2) == 10)
  })

test_that("probability function is valid",
    {
    expect_error(bin_probability(1))
    expect_error(bin_probability(10, 5, -.5))
    expect_error(bin_probability(100, 55, .5))
    })

test_that("pdf function is valid",
          {
            expect_that(class(bin_distribution(10,0.5)), equals(c("bindis", "data.frame")))
            expect_error(bin_distribution(100))
            expect_error(bin_distribution(100, -0.5))
            })

test_that("cdf function is valid",
          {
            expect_that(class(bin_cumulative(10,0.5)), equals(c("bincum", "data.frame")))
            expect_error(bin_cumulative(100))
            expect_error(bin_cumulative(100, -0.5))
          })
