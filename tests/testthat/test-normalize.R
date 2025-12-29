# Tests for normalization utilities

test_that("normalize_prob sums to 1", {
  x <- c(1, 2, 3, 4)
  result <- normalize_prob(x)

  expect_equal(sum(result), 1)
  expect_equal(result, x / sum(x))
})

test_that("normalize_prob handles zeros", {
  x <- c(0, 0, 0, 0)
  result <- normalize_prob(x)

  # Should return uniform distribution
  expect_equal(result, c(0.25, 0.25, 0.25, 0.25))
})

test_that("normalize_prob warns on negative values", {
  expect_warning(
    result <- normalize_prob(c(-1, 2, 3)),
    "Negative"
  )
  # Should set negative to 0
  expect_equal(result[1], 0)
})

test_that("normalize_rate computes correct rates", {
  count <- c(50, 100, 200)
  base <- c(10000, 50000, 100000)

  result <- normalize_rate(count, base, per = 100000)

  expect_equal(result, c(500, 200, 200))
})

test_that("normalize_rate handles zero base", {
  count <- c(50, 100)
  base <- c(10000, 0)

  result <- normalize_rate(count, base, per = 1)
  expect_true(is.na(result[2]))

  result_no_na <- normalize_rate(count, base, per = 1, na_for_zero = FALSE)
  expect_equal(result_no_na[2], 0)
})

test_that("normalize_zscore returns z-scores", {
  x <- c(10, 20, 30, 40, 50)
  z <- normalize_zscore(x)

  expect_equal(mean(z), 0, tolerance = 1e-10)
  expect_equal(sd(z), 1, tolerance = 1e-10)
})

test_that("funnel_zscore computes correct z-scores", {
  observed <- c(50, 100, 150)
  expected <- c(45, 95, 160)
  sample_size <- c(10000, 50000, 100000)

  z <- funnel_zscore(observed, expected, sample_size, type = "count")

  # Check that larger sample with same deviation has larger z
  expect_true(is.numeric(z))
  expect_equal(length(z), 3)
})

test_that("funnel_pvalue returns values in [0, 1]", {
  z <- c(-2, -1, 0, 1, 2)
  p <- funnel_pvalue(z)

  expect_true(all(p >= 0 & p <= 1))
  expect_equal(p[3], 1)  # z = 0 should give p = 1
  expect_equal(p[1], p[5])  # Symmetric
})

test_that("normalize_minmax scales to [0, 1]", {
  x <- c(10, 20, 30, 40, 50)
  result <- normalize_minmax(x)

  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
})
