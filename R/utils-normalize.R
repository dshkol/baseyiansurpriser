# Normalization Utility Functions
#
# Helper functions for normalizing data in various ways required
# by different model types.

#' Normalize to Probability Distribution
#'
#' Normalizes a numeric vector to sum to 1, creating a valid probability
#' distribution.
#'
#' @param x Numeric vector (non-negative values expected)
#' @param na.rm Logical; remove NA values before normalizing?
#'
#' @return Numeric vector summing to 1 (or containing NAs if input had NAs
#'   and na.rm = FALSE)
#'
#' @details
#' If all values are zero or if sum is zero, returns uniform distribution.
#' Negative values are set to zero with a warning.
#'
#' @export
#' @examples
#' normalize_prob(c(1, 2, 3, 4))
#' normalize_prob(c(10, 20, 30))
#' normalize_prob(c(0, 0, 0))  # Returns uniform
normalize_prob <- function(x, na.rm = FALSE) {
  if (any(x < 0, na.rm = TRUE)) {
    cli_warn("Negative values in {.arg x} set to zero.")
    x <- pmax(x, 0)
  }

  total <- sum(x, na.rm = na.rm)

  if (total == 0 || !is.finite(total)) {
    # Return uniform distribution
    n <- if (na.rm) sum(!is.na(x)) else length(x)
    result <- rep(1 / n, length(x))
    if (!na.rm) result[is.na(x)] <- NA
    return(result)
  }

  x / total
}

#' Normalize to Rate (Per Capita)
#'
#' Computes rates by dividing counts by a base value (e.g., population).
#'
#' @param count Numeric vector of counts (e.g., number of events)
#' @param base Numeric vector of base values (e.g., population)
#' @param per Multiplier for scaling (e.g., 100000 for "per 100k")
#' @param na_for_zero Logical; return NA when base is zero?
#'
#' @return Numeric vector of rates
#'
#' @export
#' @examples
#' # Crime rate per 100,000 population
#' crimes <- c(50, 100, 200)
#' population <- c(10000, 50000, 100000)
#' normalize_rate(crimes, population, per = 100000)
normalize_rate <- function(count, base, per = 1, na_for_zero = TRUE) {
  if (length(count) != length(base)) {
    cli_abort("{.arg count} and {.arg base} must have the same length.")
  }

  rate <- count / base * per

  if (na_for_zero) {
    rate[base == 0] <- NA
  } else {
    rate[!is.finite(rate)] <- 0
  }

  rate
}

#' Z-Score Normalization
#'
#' Normalizes values to z-scores (standard deviations from mean).
#'
#' @param x Numeric vector
#' @param center Value to center around (default: mean of x)
#' @param scale Scale factor (default: standard deviation of x)
#' @param na.rm Logical; remove NA values when computing center/scale?
#'
#' @return Numeric vector of z-scores
#'
#' @export
#' @examples
#' x <- c(10, 20, 30, 40, 50)
#' normalize_zscore(x)
normalize_zscore <- function(x, center = NULL, scale = NULL, na.rm = TRUE) {
  if (is.null(center)) {
    center <- mean(x, na.rm = na.rm)
  }
  if (is.null(scale)) {
    scale <- sd(x, na.rm = na.rm)
  }

  if (scale == 0 || !is.finite(scale)) {
    cli_warn("Scale is zero or non-finite; returning zeros.")
    return(rep(0, length(x)))
  }

  (x - center) / scale
}

#' Funnel Z-Score (de Moivre)
#'
#' Computes z-scores accounting for sampling variability using de Moivre's
#' equation. This normalizes observed values by their expected standard error.
#'
#' @param observed Numeric vector of observed counts or rates
#' @param expected Numeric vector of expected values (e.g., from base rate)
#' @param sample_size Numeric vector of sample sizes (e.g., population)
#' @param type Type of data: "count" (Poisson) or "proportion" (binomial)
#'
#' @return Numeric vector of z-scores
#'
#' @details
#' For count data (Poisson), SE = sqrt(expected).
#' For proportion data (binomial), SE = sqrt(p * (1-p) / n).
#'
#' Larger sample sizes result in smaller standard errors, so the same
#' deviation is more "surprising" for larger samples.
#'
#' @export
#' @examples
#' # Observed crimes vs expected (from overall rate)
#' observed <- c(50, 100, 150)
#' expected <- c(45, 95, 160)
#' sample_size <- c(10000, 50000, 100000)
#' funnel_zscore(observed, expected, sample_size, type = "count")
funnel_zscore <- function(observed, expected, sample_size,
                          type = c("count", "proportion")) {
  type <- match.arg(type)

  n <- length(observed)
  if (length(expected) != n || length(sample_size) != n) {
    cli_abort("All input vectors must have the same length.")
  }

  se <- switch(type,
    count = {
      # Poisson: SE = sqrt(expected)
      sqrt(pmax(expected, 0.5))  # Avoid SE = 0
    },
    proportion = {
      # Binomial: SE = sqrt(p * (1-p) / n)
      p <- expected / sample_size
      p <- pmin(pmax(p, 0.001), 0.999)  # Bound away from 0 and 1
      sqrt(p * (1 - p) / sample_size)
    }
  )

  z <- (observed - expected) / se
  z[!is.finite(z)] <- 0

  z
}

#' Compute P-Value from Funnel Z-Score
#'
#' Converts z-scores to two-tailed p-values under the normal distribution.
#'
#' @param z Numeric vector of z-scores
#'
#' @return Numeric vector of p-values
#'
#' @export
#' @examples
#' z <- c(-2, -1, 0, 1, 2)
#' funnel_pvalue(z)
funnel_pvalue <- function(z) {
  2 * (1 - stats::pnorm(abs(z)))
}

#' Min-Max Normalization
#'
#' Scales values to the range 0 to 1 using min-max normalization.
#'
#' @param x Numeric vector
#' @param min_val Minimum value for scaling (default: min of x)
#' @param max_val Maximum value for scaling (default: max of x)
#' @param na.rm Logical; remove NA values when computing range?
#'
#' @return Numeric vector scaled to the range 0 to 1
#'
#' @export
#' @examples
#' normalize_minmax(c(10, 20, 30, 40, 50))
#' normalize_minmax(c(-5, 0, 5), min_val = -10, max_val = 10)
normalize_minmax <- function(x, min_val = NULL, max_val = NULL, na.rm = TRUE) {
  if (is.null(min_val)) min_val <- min(x, na.rm = na.rm)
  if (is.null(max_val)) max_val <- max(x, na.rm = na.rm)

  range_val <- max_val - min_val
  if (range_val == 0 || !is.finite(range_val)) {
    cli_warn("Range is zero or non-finite; returning 0.5.")
    return(rep(0.5, length(x)))
  }

  (x - min_val) / range_val
}

#' Robust Normalization (using quantiles)
#'
#' Scales values using quantiles instead of min/max for robustness to outliers.
#'
#' @param x Numeric vector
#' @param lower_quantile Lower quantile for scaling (default: 0.05)
#' @param upper_quantile Upper quantile for scaling (default: 0.95)
#' @param na.rm Logical; remove NA values when computing quantiles?
#'
#' @return Numeric vector scaled approximately to the range 0 to 1, with outliers
#'   potentially outside this range
#'
#' @export
#' @examples
#' # With outliers
#' x <- c(1, 2, 3, 4, 5, 100)  # 100 is an outlier
#' normalize_robust(x)
normalize_robust <- function(x, lower_quantile = 0.05, upper_quantile = 0.95,
                              na.rm = TRUE) {
  quantiles <- stats::quantile(x, c(lower_quantile, upper_quantile), na.rm = na.rm)
  normalize_minmax(x, min_val = quantiles[1], max_val = quantiles[2], na.rm = na.rm)
}
