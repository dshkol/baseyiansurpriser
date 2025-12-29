# Uniform Model
#
# Model assuming equiprobable events across all regions.

#' Create a Uniform Model
#'
#' Creates a model that assumes events are equally likely across all regions.
#' This serves as a "null hypothesis" baseline - regions where events cluster
#' will show high surprise under this model.
#'
#' @param n_regions Number of regions (optional, inferred from data if NULL)
#'
#' @return A `bs_model_uniform` object
#'
#' @details
#' Under the uniform model, expected probability for each region is 1/n,
#' where n is the total number of regions. The likelihood is computed as:
#'
#' \deqn{P(D|Uniform) = 1 - \frac{1}{2} \sum_i |O_i - \frac{1}{n}|}
#'
#' This is the Total Variation Distance from uniform, transformed to a probability.
#'
#' The uniform model is useful for detecting spatial clustering - any
#' concentration of events in fewer regions will produce high surprise.
#'
#' @export
#' @examples
#' # Create uniform model
#' model <- bs_model_uniform()
#'
#' # The model computes likelihood when used in a model space
#' space <- model_space(model)
bs_model_uniform <- function(n_regions = NULL) {

  likelihood_fn <- function(observed, region_idx = NULL, ...) {
    n <- length(observed)
    expected <- rep(1 / n, n)

    if (!is.null(region_idx)) {
      # Per-region likelihood
      # How likely is this region's value under uniform assumption?
      # Using multinomial/Poisson-like approach
      total <- sum(observed, na.rm = TRUE)
      if (total == 0) return(0)

      # Expected count for this region under uniform
      expected_count <- total / n

      # Log-likelihood contribution from this region
      # Using Poisson approximation
      obs_i <- observed[region_idx]
      if (is.na(obs_i)) return(-Inf)

      stats::dpois(round(obs_i), lambda = expected_count, log = TRUE)
    } else {
      # Global likelihood
      # Total variation distance from uniform, converted to log-likelihood
      observed_prop <- normalize_prob(observed)
      tvd <- 0.5 * sum(abs(observed_prop - expected))
      # Convert TVD to pseudo-likelihood (1 - TVD is probability-like)
      log(pmax(1 - tvd, 1e-10))
    }
  }

  new_bs_model(
    type = "uniform",
    params = list(n_regions = n_regions),
    likelihood_fn = likelihood_fn,
    name = "Uniform"
  )
}
