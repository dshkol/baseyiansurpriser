# Gaussian Model
#
# Parametric model using Gaussian (normal) distribution.

#' Create a Gaussian Model
#'
#' Creates a model based on a Gaussian (normal) distribution. This parametric
#' model is useful for detecting outliers and identifying multiple modes in
#' data.
#'
#' @param mu Mean of the Gaussian. If NULL, estimated from data.
#' @param sigma Standard deviation. If NULL, estimated from data.
#' @param fit_from_data Logical; estimate parameters from data?
#' @param name Optional name for the model
#'
#' @return A `bs_model_gaussian` object
#'
#' @details
#' The Gaussian model assumes data is drawn from a normal distribution.
#' Points far from the mean (in terms of standard deviations) will have
#' low likelihood and thus create high surprise when this model has
#' probability mass.
#'
#' This model is particularly useful for:
#' - Detecting spatial outliers
#' - Identifying multi-modal distributions
#' - Combating renormalization bias (outliers get suppressed in dynamic
#'   visualizations)
#'
#' @export
#' @examples
#' # Gaussian model with parameters fit from data
#' model <- bs_model_gaussian()
#'
#' # Gaussian model with fixed parameters
#' model <- bs_model_gaussian(mu = 100, sigma = 20)
#'
#' # Use in model space with other models
#' population <- c(10000, 50000, 100000, 25000)
#' space <- model_space(
#'   bs_model_uniform(),
#'   bs_model_baserate(population),
#'   bs_model_gaussian()
#' )
bs_model_gaussian <- function(mu = NULL, sigma = NULL,
                               fit_from_data = TRUE,
                               name = NULL) {

  likelihood_fn <- function(observed, region_idx = NULL, ...) {
    # Estimate parameters if needed
    if (fit_from_data || is.null(mu) || is.null(sigma)) {
      mu_est <- mean(observed, na.rm = TRUE)
      sigma_est <- sd(observed, na.rm = TRUE)
      # Handle constant data
      if (is.na(sigma_est) || sigma_est == 0) {
        sigma_est <- 1
      }
    } else {
      mu_est <- mu
      sigma_est <- sigma
    }

    if (!is.null(region_idx)) {
      # Per-region likelihood
      obs_i <- observed[region_idx]
      if (is.na(obs_i)) return(-Inf)

      stats::dnorm(obs_i, mean = mu_est, sd = sigma_est, log = TRUE)
    } else {
      # Global likelihood - sum of log densities
      valid <- !is.na(observed)
      sum(stats::dnorm(observed[valid], mean = mu_est, sd = sigma_est, log = TRUE))
    }
  }

  new_bs_model(
    type = "gaussian",
    params = list(mu = mu, sigma = sigma, fit_from_data = fit_from_data),
    likelihood_fn = likelihood_fn,
    name = name %||% "Gaussian"
  )
}

#' Create Multi-Modal Gaussian Mixture Model
#'
#' Creates a model based on a mixture of Gaussians for multi-modal data.
#'
#' @param means Vector of means for each component
#' @param sds Vector of standard deviations for each component
#' @param weights Vector of mixture weights (must sum to 1)
#' @param name Optional name for the model
#'
#' @return A `bs_model_gaussian_mixture` object
#'
#' @export
#' @examples
#' # Two-component mixture
#' model <- bs_model_gaussian_mixture(
#'   means = c(50, 150),
#'   sds = c(10, 20),
#'   weights = c(0.7, 0.3)
#' )
bs_model_gaussian_mixture <- function(means, sds, weights = NULL,
                                       name = NULL) {
  k <- length(means)

  if (length(sds) != k) {
    cli_abort("{.arg means} and {.arg sds} must have the same length.")
  }

  if (is.null(weights)) {
    weights <- rep(1 / k, k)
  }
  weights <- normalize_prob(weights)

  likelihood_fn <- function(observed, region_idx = NULL, ...) {
    if (!is.null(region_idx)) {
      obs_i <- observed[region_idx]
      if (is.na(obs_i)) return(-Inf)

      # Log of mixture density
      log_components <- vapply(seq_len(k), function(j) {
        log(weights[j]) + stats::dnorm(obs_i, mean = means[j], sd = sds[j], log = TRUE)
      }, numeric(1))
      log_sum_exp(log_components)
    } else {
      valid <- !is.na(observed)
      total <- 0
      for (i in which(valid)) {
        log_components <- vapply(seq_len(k), function(j) {
          log(weights[j]) + stats::dnorm(observed[i], mean = means[j], sd = sds[j], log = TRUE)
        }, numeric(1))
        total <- total + log_sum_exp(log_components)
      }
      total
    }
  }

  new_bs_model(
    type = "gaussian_mixture",
    params = list(means = means, sds = sds, weights = weights, k = k),
    likelihood_fn = likelihood_fn,
    name = name %||% paste0("Gaussian Mixture (k=", k, ")")
  )
}
