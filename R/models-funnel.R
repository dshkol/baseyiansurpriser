# de Moivre Funnel Model
#
# Model that accounts for sampling variation using standard error normalization.

#' Create a de Moivre Funnel Model
#'
#' Creates a model that normalizes observations by their expected standard error,
#' accounting for varying sample sizes. This addresses "sampling error bias"
#' where regions with small sample sizes show artificially high variability.
#'
#' @param sample_size Numeric vector of sample sizes (e.g., population)
#' @param target_rate Target rate/proportion. If NULL, estimated from data.
#' @param type Type of data: "count" (Poisson) or "proportion" (binomial)
#' @param control_limits Numeric vector of control limits (in SDs) for funnel plot.
#'   Default is c(2, 3) for warning and control limits.
#' @param name Optional name for the model
#'
#' @return A `bs_model_funnel` object
#'
#' @details
#' The de Moivre funnel model uses the insight that sampling variability
#' decreases with sample size according to de Moivre's equation:
#' \deqn{SE = \sigma / \sqrt{n}}
#'
#' For count data (Poisson), the model computes z-scores as:
#' \deqn{z = (observed - expected) / \sqrt{expected}}
#'
#' For proportion data (binomial):
#' \deqn{z = (observed - expected) / \sqrt{p(1-p)/n}}
#'
#' Observations with large z-scores (far from expected after accounting for
#' sample size) are genuinely surprising, while high rates in small regions
#' are discounted as expected variation.
#'
#' This model is essential for:
#' - De-biasing per-capita rate maps
#' - Creating funnel plots
#' - Identifying genuine outliers vs. sampling noise
#'
#' @export
#' @examples
#' # Population sizes for regions
#' population <- c(10000, 50000, 100000, 25000)
#'
#' # Funnel model for count data
#' model <- bs_model_funnel(population)
#'
#' # Funnel model with known target rate
#' model <- bs_model_funnel(population, target_rate = 0.001)
#'
#' # For proportion data
#' model <- bs_model_funnel(population, type = "proportion")
bs_model_funnel <- function(sample_size,
                             target_rate = NULL,
                             type = c("count", "proportion"),
                             control_limits = c(2, 3),
                             name = NULL) {
  type <- match.arg(type)

  if (is.null(sample_size) || length(sample_size) == 0) {
    cli_abort("{.arg sample_size} cannot be NULL or empty.")
  }

  likelihood_fn <- function(observed, region_idx = NULL, ...) {
    n_regions <- length(observed)

    # Handle sample_size length mismatch
    ss <- if (length(sample_size) == 1) {
      rep(sample_size, n_regions)
    } else if (length(sample_size) != n_regions) {
      cli_abort("sample_size length ({length(sample_size)}) must match observed ({n_regions})")
    } else {
      sample_size
    }

    # Compute target rate if not provided
    rate <- target_rate
    if (is.null(rate)) {
      rate <- sum(observed, na.rm = TRUE) / sum(ss, na.rm = TRUE)
    }

    # Compute expected values and standard errors
    expected <- ss * rate

    se <- switch(type,
      count = {
        # Poisson: SE = sqrt(expected)
        sqrt(pmax(expected, 0.5))
      },
      proportion = {
        # Binomial: SE = sqrt(p * (1-p) / n)
        p <- pmin(pmax(rate, 0.001), 0.999)
        sqrt(p * (1 - p) * ss)
      }
    )

    if (!is.null(region_idx)) {
      # Per-region likelihood
      obs_i <- observed[region_idx]
      if (is.na(obs_i)) return(-Inf)

      # Z-score for this region
      z <- (obs_i - expected[region_idx]) / se[region_idx]

      # Likelihood under standard normal
      stats::dnorm(z, log = TRUE)
    } else {
      # Global likelihood
      valid <- !is.na(observed)
      z <- (observed[valid] - expected[valid]) / se[valid]
      sum(stats::dnorm(z, log = TRUE))
    }
  }

  new_bs_model(
    type = "funnel",
    params = list(
      sample_size = sample_size,
      target_rate = target_rate,
      type = type,
      control_limits = control_limits
    ),
    likelihood_fn = likelihood_fn,
    name = name %||% "de Moivre Funnel"
  )
}

#' Create Funnel Model from Column
#'
#' Convenience function to create a funnel model from a data frame column.
#'
#' @param data Data frame or sf object
#' @param column Name of column containing sample sizes
#' @param ... Additional arguments passed to [bs_model_funnel()]
#'
#' @return A `bs_model_funnel` object
#'
#' @export
#' @examples
#' # Using sf package's NC data
#' # library(sf)
#' # nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' # model <- bs_model_funnel_col(nc, "BIR74")
bs_model_funnel_col <- function(data, column, ...) {
  if (!column %in% names(data)) {
    cli_abort("Column {.val {column}} not found in data.")
  }

  sample_size <- data[[column]]
  bs_model_funnel(sample_size, name = paste("Funnel:", column), ...)
}

#' Compute Funnel Plot Data
#'
#' Computes the data needed to create a funnel plot, including control limits.
#'
#' @param observed Numeric vector of observed values
#' @param sample_size Numeric vector of sample sizes
#' @param target_rate Target rate. If NULL, computed from data.
#' @param type Type of data: "count" or "proportion"
#' @param limits Vector of SD multiples for control limits (default: c(2, 3))
#'
#' @return A data frame with columns: observed, sample_size, expected, z_score,
#'   and control limit columns (lower_2sd, upper_2sd, lower_3sd, upper_3sd, etc.)
#'
#' @export
#' @examples
#' observed <- c(50, 100, 150, 200)
#' sample_size <- c(10000, 50000, 100000, 25000)
#' funnel_data <- compute_funnel_data(observed, sample_size)
compute_funnel_data <- function(observed, sample_size,
                                 target_rate = NULL,
                                 type = c("count", "proportion"),
                                 limits = c(2, 3)) {
  type <- match.arg(type)
  n <- length(observed)

  if (length(sample_size) != n) {
    cli_abort("{.arg observed} and {.arg sample_size} must have same length.")
  }

  # Compute target rate
  rate <- target_rate %||% (sum(observed, na.rm = TRUE) / sum(sample_size, na.rm = TRUE))

  # Compute expected and SE
  expected <- sample_size * rate
  se <- switch(type,
    count = sqrt(pmax(expected, 0.5)),
    proportion = {
      p <- pmin(pmax(rate, 0.001), 0.999)
      sqrt(p * (1 - p) * sample_size)
    }
  )

  # Z-scores
  z_score <- (observed - expected) / se

  # Build result data frame
  result <- data.frame(
    observed = observed,
    sample_size = sample_size,
    expected = expected,
    se = se,
    z_score = z_score
  )

  # Add control limits
  for (lim in limits) {
    result[[paste0("lower_", lim, "sd")]] <- expected - lim * se
    result[[paste0("upper_", lim, "sd")]] <- expected + lim * se
  }

  result
}
