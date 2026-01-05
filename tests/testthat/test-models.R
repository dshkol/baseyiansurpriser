# Tests for model constructors

test_that("bs_model_uniform creates valid model", {
  model <- bs_model_uniform()
  expect_s3_class(model, "bs_model_uniform")
  expect_s3_class(model, "bs_model")
  expect_true(is.function(model$compute_likelihood))
  expect_equal(model$type, "uniform")
})

test_that("bs_model_baserate creates valid model", {
  expected <- c(100, 200, 300, 400)
  model <- bs_model_baserate(expected)

  expect_s3_class(model, "bs_model_baserate")
  expect_s3_class(model, "bs_model")
  expect_equal(model$type, "baserate")

  # Check normalization
  expect_equal(sum(model$params$expected_prop), 1)
})

test_that("bs_model_gaussian creates valid model", {
  model <- bs_model_gaussian()
  expect_s3_class(model, "bs_model_gaussian")
  expect_true(model$params$fit_from_data)

  model_fixed <- bs_model_gaussian(mu = 50, sigma = 10, fit_from_data = FALSE)
  expect_equal(model_fixed$params$mu, 50)
  expect_equal(model_fixed$params$sigma, 10)
})

test_that("bs_model_sampled creates valid model", {
  model <- bs_model_sampled(sample_frac = 0.2)
  expect_s3_class(model, "bs_model_sampled")
  expect_equal(model$params$sample_frac, 0.2)
})

test_that("bs_model_funnel creates valid model", {
  sample_sizes <- c(10000, 50000, 100000, 25000)
  model <- bs_model_funnel(sample_sizes)

  expect_s3_class(model, "bs_model_funnel")
  expect_equal(model$params$type, "count")
  expect_equal(model$params$sample_size, sample_sizes)
})

test_that("model_space combines models correctly", {
  space <- model_space(
    bs_model_uniform(),
    bs_model_gaussian()
  )

  expect_s3_class(space, "bs_model_space")
  expect_equal(space$n_models, 2)
  expect_equal(sum(space$prior), 1)
  expect_equal(unname(space$prior), c(0.5, 0.5))
})

test_that("model_space accepts custom prior", {
  space <- model_space(
    bs_model_uniform(),
    bs_model_gaussian(),
    prior = c(0.3, 0.7)
  )

  expect_equal(unname(space$prior), c(0.3, 0.7))
})

test_that("default_model_space creates appropriate models", {
  expected <- c(100, 200, 300, 400)
  space <- default_model_space(expected)

  expect_equal(space$n_models, 3)
  expect_equal(names(space$models), c("Uniform", "Base Rate", "de Moivre Funnel (paper)"))
})
