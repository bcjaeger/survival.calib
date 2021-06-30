

x_values <- c()

for(i in 1:5)
 x_values <- c(x_values, rep(i, times = 5))

test_that(
 desc = "basic example works",
 code = {
  expect_equal(cut_percentiles(x = x_values, g = 5), x_values)
 }
)

test_that(
 desc = "g too high causes error",
 code = {
  expect_error(
   object = cut_percentiles(x_values, g = 6),
   regexp = "number of unique values in x"
  )
 }
)



