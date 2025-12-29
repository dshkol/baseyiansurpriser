# Sampled Subset Model (KDE)
#
# Non-parametric model using kernel density estimation.

#' Create a Sampled Subset Model (KDE)
#'
#' Creates a non-parametric model using kernel density estimation (KDE).
#' This model is built from a sample of the data and can detect when
#' subsequent observations deviate from the pattern established by early data.
#'
#' @param sample_frac Fraction of data to use for building the prior (0 < x < 1).
#'   If NULL, uses all data for density estimation.
#' @param kernel Kernel type for density estimation.
#'   One of: "gaussian", "epanechnikov", "rectangular", "triangular", "biweight",
#'   "cosine", "optcosine"
#' @param bandwidth Bandwidth selection method or numeric value.
#'   If character, one of: "nrd0", "nrd", "ucv", "bcv", "SJ".
#'   If numeric, used directly as bandwidth.
#' @param sample_indices Integer vector of specific indices to use for building prior.
#'   Overrides `sample_frac` if provided.
#' @param name Optional name for the model
#'
#' @return A `bs_model_sampled` object
#'
#' @details
#' The sampled model builds a density estimate from a subset of observations
#' (typically early observations in temporal data) and measures surprise as
#' deviation from this learned distribution.
#'
#' This is useful for:
#' - Detecting temporal changes in distribution
#' - Building a "post hoc" model from initial observations
#' - Detecting emerging patterns in streaming data
#'
#' The likelihood for each observation is the density at that point under
#' the KDE built from the sample.
#'
#' @export
#' @examples
#' # KDE model using first 10% of data
#' model <- bs_model_sampled(sample_frac = 0.1)
#'
#' # KDE with specific bandwidth
#' model <- bs_model_sampled(bandwidth = 5)
#'
#' # Use specific observations for training
#' model <- bs_model_sampled(sample_indices = 1:10)
bs_model_sampled <- function(sample_frac = NULL,
                              kernel = "gaussian",
                              bandwidth = "nrd0",
                              sample_indices = NULL,
                              name = NULL) {

  # Store density estimate once computed
  cached_density <- NULL

  likelihood_fn <- function(observed, region_idx = NULL, ...) {
    n <- length(observed)

    # Determine which observations to use for density estimation
    if (!is.null(sample_indices)) {
      train_idx <- sample_indices
    } else if (!is.null(sample_frac)) {
      n_sample <- max(3, floor(n * sample_frac))
      train_idx <- seq_len(n_sample)
    } else {
      # Use all data
      train_idx <- seq_len(n)
    }

    train_data <- observed[train_idx]
    train_data <- train_data[!is.na(train_data)]

    if (length(train_data) < 2) {
      cli_warn("Insufficient data for density estimation; returning uniform likelihood.")
      if (!is.null(region_idx)) return(0)
      return(-n * log(n))
    }

    # Compute density estimate
    bw <- if (is.numeric(bandwidth)) {
      bandwidth
    } else {
      tryCatch(
        stats::bw.nrd0(train_data),
        error = function(e) 1
      )
    }

    dens <- stats::density(train_data, bw = bw, kernel = kernel,
                            n = 512, from = min(train_data) - 3 * bw,
                            to = max(train_data) + 3 * bw)

    if (!is.null(region_idx)) {
      # Per-region likelihood
      obs_i <- observed[region_idx]
      if (is.na(obs_i)) return(-Inf)

      # Evaluate density at this point
      dens_val <- stats::approx(dens$x, dens$y, xout = obs_i, rule = 2)$y
      dens_val <- pmax(dens_val, 1e-10)
      log(dens_val)
    } else {
      # Global likelihood
      valid <- !is.na(observed)
      dens_vals <- stats::approx(dens$x, dens$y, xout = observed[valid], rule = 2)$y
      dens_vals <- pmax(dens_vals, 1e-10)
      sum(log(dens_vals))
    }
  }

  new_bs_model(
    type = "sampled",
    params = list(
      sample_frac = sample_frac,
      kernel = kernel,
      bandwidth = bandwidth,
      sample_indices = sample_indices
    ),
    likelihood_fn = likelihood_fn,
    name = name %||% "Sampled (KDE)"
  )
}

#' Create a Bootstrap Sample Model
#'
#' Creates a model based on a bootstrap sample of the data.
#' Useful for assessing variability and identifying observations
#' that are surprising relative to resampled distributions.
#'
#' @param n_bootstrap Number of bootstrap samples to use
#' @param seed Random seed for reproducibility
#' @param name Optional name for the model
#'
#' @return A `bs_model_bootstrap` object
#'
#' @export
#' @examples
#' model <- bs_model_bootstrap(n_bootstrap = 100)
bs_model_bootstrap <- function(n_bootstrap = 100, seed = NULL, name = NULL) {

  likelihood_fn <- function(observed, region_idx = NULL, ...) {
    if (!is.null(seed)) set.seed(seed)

    n <- length(observed)
    valid <- !is.na(observed)
    obs_valid <- observed[valid]

    if (length(obs_valid) < 3) {
      if (!is.null(region_idx)) return(0)
      return(-n * log(n))
    }

    # Generate bootstrap samples and compute densities
    boot_densities <- replicate(n_bootstrap, {
      boot_sample <- sample(obs_valid, replace = TRUE)
      bw <- stats::bw.nrd0(boot_sample)
      stats::density(boot_sample, bw = bw, n = 256)
    }, simplify = FALSE)

    if (!is.null(region_idx)) {
      obs_i <- observed[region_idx]
      if (is.na(obs_i)) return(-Inf)

      # Average density across bootstrap samples
      avg_dens <- mean(vapply(boot_densities, function(d) {
        stats::approx(d$x, d$y, xout = obs_i, rule = 2)$y
      }, numeric(1)))
      avg_dens <- pmax(avg_dens, 1e-10)
      log(avg_dens)
    } else {
      # Global likelihood
      total <- 0
      for (i in which(valid)) {
        avg_dens <- mean(vapply(boot_densities, function(d) {
          stats::approx(d$x, d$y, xout = observed[i], rule = 2)$y
        }, numeric(1)))
        avg_dens <- pmax(avg_dens, 1e-10)
        total <- total + log(avg_dens)
      }
      total
    }
  }

  new_bs_model(
    type = "bootstrap",
    params = list(n_bootstrap = n_bootstrap, seed = seed),
    likelihood_fn = likelihood_fn,
    name = name %||% paste0("Bootstrap (n=", n_bootstrap, ")")
  )
}
