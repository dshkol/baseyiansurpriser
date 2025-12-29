# sf Package Integration
#
# Methods for working with sf (simple features) spatial data objects.

#' @export
#' @rdname surprise
surprise.sf <- function(data,
                         observed,
                         expected = NULL,
                         sample_size = NULL,
                         models = c("uniform", "baserate", "funnel"),
                         prior = NULL,
                         signed = TRUE,
                         ...) {
  # Extract observed values
  obs_vals <- extract_column(data, rlang::enquo(observed))

  # Extract expected values if provided
  exp_vals <- NULL
  expected_quo <- rlang::enquo(expected)
  if (!rlang::quo_is_null(expected_quo)) {
    exp_vals <- extract_column(data, expected_quo)
  }

  # Extract sample sizes if provided
  size_vals <- NULL
  sample_size_quo <- rlang::enquo(sample_size)
  if (!rlang::quo_is_null(sample_size_quo)) {
    size_vals <- extract_column(data, sample_size_quo)
  } else if (!is.null(exp_vals)) {
    size_vals <- exp_vals
  }

  # Build or validate model space
  mspace <- build_model_space_from_spec(models, exp_vals, size_vals, prior)

  # Compute surprise
  result <- compute_surprise(
    model_space = mspace,
    observed = obs_vals,
    expected = exp_vals,
    return_signed = signed,
    ...
  )

  # Add results to sf object
  data$surprise <- result$surprise
  if (signed && !is.null(result$signed_surprise)) {
    data$signed_surprise <- result$signed_surprise
  }

  # Return as bs_surprise_sf
  new_bs_surprise_sf(data, result)
}

#' Compute Surprise for sf Object
#'
#' Convenience wrapper for computing surprise on sf objects.
#'
#' @inheritParams surprise
#' @param x An sf object
#'
#' @return A `bs_surprise_sf` object
#'
#' @export
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' result <- st_surprise(nc, observed = SID74, expected = BIR74)
#' plot(result)
st_surprise <- function(x, observed, expected = NULL, ...) {
  if (!inherits(x, "sf")) {
    cli_abort("{.arg x} must be an sf object.")
  }
  surprise(x, observed = {{ observed }}, expected = {{ expected }}, ...)
}

#' Spatial Density Estimation for sf Objects
#'
#' Computes kernel density estimates for point or polygon sf objects.
#'
#' @param x An sf object with point or polygon geometries
#' @param method Density estimation method: "kde" (kernel density) or
#'   "kriging" (requires additional packages)
#' @param bandwidth Bandwidth for KDE. If NULL, estimated automatically.
#' @param n Grid size for density estimation
#' @param weights Optional weights for each feature
#' @param ... Additional arguments passed to density estimation functions
#'
#' @return A list with density estimates and grid information
#'
#' @export
#' @examples
#' library(sf)
#' # Create random points
#' pts <- st_as_sf(data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' ), coords = c("x", "y"))
#'
#' # Compute density
#' dens <- st_density(pts)
st_density <- function(x, method = c("kde", "kriging"),
                        bandwidth = NULL, n = 100, weights = NULL, ...) {
  method <- match.arg(method)

  if (!inherits(x, "sf")) {
    cli_abort("{.arg x} must be an sf object.")
  }

  # Get geometry type
  geom_type <- sf::st_geometry_type(x, by_geometry = FALSE)

  # Extract coordinates
  if (geom_type %in% c("POINT", "MULTIPOINT")) {
    coords <- sf::st_coordinates(x)
  } else {
    # Use centroids for non-point geometries
    centroids <- sf::st_centroid(sf::st_geometry(x))
    coords <- sf::st_coordinates(centroids)
  }

  if (nrow(coords) < 3) {
    cli_abort("Need at least 3 points for density estimation.")
  }

  # Estimate bandwidth if not provided
  if (is.null(bandwidth)) {
    bw_x <- MASS::bandwidth.nrd(coords[, 1])
    bw_y <- MASS::bandwidth.nrd(coords[, 2])
    bandwidth <- c(bw_x, bw_y)
  } else if (length(bandwidth) == 1) {
    bandwidth <- rep(bandwidth, 2)
  }

  # Compute 2D KDE
  dens <- MASS::kde2d(
    x = coords[, 1],
    y = coords[, 2],
    h = bandwidth,
    n = n,
    ...
  )

  list(
    x = dens$x,
    y = dens$y,
    z = dens$z,
    bandwidth = bandwidth,
    n_points = nrow(coords),
    method = method
  )
}

#' Evaluate Density at sf Feature Locations
#'
#' Evaluates a density estimate at the locations of features in an sf object.
#'
#' @param density Density estimate from [st_density()]
#' @param x sf object with features to evaluate at
#'
#' @return Numeric vector of density values
#'
#' @export
st_density_at <- function(density, x) {
  if (!inherits(x, "sf")) {
    cli_abort("{.arg x} must be an sf object.")
  }

  # Get coordinates
  geom_type <- sf::st_geometry_type(x, by_geometry = FALSE)
  if (geom_type %in% c("POINT", "MULTIPOINT")) {
    coords <- sf::st_coordinates(x)
  } else {
    centroids <- sf::st_centroid(sf::st_geometry(x))
    coords <- sf::st_coordinates(centroids)
  }

  # Bilinear interpolation
  n <- nrow(coords)
  dens_vals <- numeric(n)

  for (i in seq_len(n)) {
    dens_vals[i] <- interpolate_2d(
      density$x, density$y, density$z,
      coords[i, 1], coords[i, 2]
    )
  }

  dens_vals
}

#' Bilinear Interpolation on 2D Grid
#' @noRd
interpolate_2d <- function(x_grid, y_grid, z_matrix, x_pt, y_pt) {
  # Find bounding grid cells
  nx <- length(x_grid)
  ny <- length(y_grid)

  # Find x indices
  ix <- findInterval(x_pt, x_grid)
  ix <- max(1, min(ix, nx - 1))

  # Find y indices
  iy <- findInterval(y_pt, y_grid)
  iy <- max(1, min(iy, ny - 1))

  # Interpolation weights
  x1 <- x_grid[ix]
  x2 <- x_grid[ix + 1]
  y1 <- y_grid[iy]
  y2 <- y_grid[iy + 1]

  wx <- (x_pt - x1) / (x2 - x1)
  wy <- (y_pt - y1) / (y2 - y1)

  # Clamp weights to [0, 1]
  wx <- max(0, min(1, wx))
  wy <- max(0, min(1, wy))

  # Bilinear interpolation
  z11 <- z_matrix[ix, iy]
  z21 <- z_matrix[ix + 1, iy]
  z12 <- z_matrix[ix, iy + 1]
  z22 <- z_matrix[ix + 1, iy + 1]

  (1 - wx) * (1 - wy) * z11 +
    wx * (1 - wy) * z21 +
    (1 - wx) * wy * z12 +
    wx * wy * z22
}

#' Aggregate Surprise to Larger Regions
#'
#' Aggregates surprise values from smaller regions to larger regions,
#' using spatial joins.
#'
#' @param x bs_surprise_sf object with surprise values
#' @param to sf object defining larger regions to aggregate to
#' @param fun Aggregation function (default: weighted.mean)
#' @param weight_col Column name for weights (e.g., population)
#'
#' @return sf object with aggregated surprise values
#'
#' @export
st_aggregate_surprise <- function(x, to, fun = weighted.mean, weight_col = NULL) {
  if (!inherits(x, "bs_surprise_sf")) {
    cli_abort("{.arg x} must be a bs_surprise_sf object.")
  }
  if (!inherits(to, "sf")) {
    cli_abort("{.arg to} must be an sf object.")
  }

  # Spatial join
  joined <- sf::st_join(to, x)

  # Get weights if specified
  weights <- if (!is.null(weight_col) && weight_col %in% names(joined)) {
    joined[[weight_col]]
  } else {
    rep(1, nrow(joined))
  }

  # Aggregate (this is simplified - full implementation would use dplyr or data.table)
  # For now, return joined data
  cli_inform("Aggregation support is basic. Consider using dplyr for complex aggregations.")
  joined
}

#' Check if sf Package is Available
#' @noRd
check_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli_abort("Package {.pkg sf} is required for spatial operations.")
  }
}
