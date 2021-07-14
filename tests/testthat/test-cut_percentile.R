

x_values <- c()

for(i in 1:5)
 x_values <- c(x_values, rep(i, times = 5))

test_that(
  desc = "boundary errors; upper",
  code = {
    expect_error(
      predrisk_grp_prcnt(x = x_values, g = 5),
      regexp = "at least one value that is > 1"
    )
  }
)

test_that(
  desc = "boundary errors; lower",
  code = {
    expect_error(
      predrisk_grp_prcnt(x = -1*x_values, g = 5),
      regexp = "at least one value that is < 0"
    )
  }
)

test_that(
 desc = "basic example works",
 code = {
  expect_equal(predrisk_grp_prcnt(x = x_values/5, g = 5), x_values)
 }
)

test_that(
 desc = "g too high causes error",
 code = {
  expect_error(
   object = predrisk_grp_prcnt(x_values/5, g = 6),
   regexp = "number of unique values in x"
  )
 }
)





