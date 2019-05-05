#context?

test_that("probability checker function is valid",
          {expect_error(check_prob(1, 2))
          expect_error(check_prob(1))
          expect_error(check_prob(-1))
                         })
test_that("trials checker function is valid",
          {expect_error(check_trials(-1))
          expect_error(check_trials(1,2))
          expect_true(check_trials(1))
          })

test_that("success checker function is valid",
          {expect_error(check_success(1,2))
          expect_error(check_success(1))
          expect_error(check_success(1, -2))
          })


