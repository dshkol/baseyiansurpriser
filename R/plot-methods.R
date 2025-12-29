# Base R Plot Methods
#
# Plot methods for bs_surprise objects using base graphics and sf.

#' Plot Surprise Map (sf)
#'
#' Plots a surprise map using sf's plot method. For ggplot2 integration,
#' see [geom_surprise()] and [scale_fill_surprise()].
#'
#' @param x A `bs_surprise_sf` object
#' @param y Unused (for S3 method compatibility)
#' @param which Which variable to plot: "surprise" or "signed_surprise"
#' @param pal Color palette. If NULL, uses default diverging palette for
#'   signed surprise and sequential for unsigned.
#' @param breaks Breaks for color scale. If NULL, uses pretty breaks.
#' @param nbreaks Number of breaks if `breaks` is NULL
#' @param main Plot title. If NULL, auto-generated.
#' @param border Border color for polygons
#' @param lwd Line width for borders
#' @param key.pos Position of legend (1=below, 2=left, 3=above, 4=right, NULL=none)
#' @param ... Additional arguments passed to [sf::plot()]
#'
#' @return Invisibly returns the input object
#'
#' @export
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' result <- surprise(nc, observed = SID74, expected = BIR74)
#'
#' # Default plot
#' plot(result)
#'
#' # Plot signed surprise
#' plot(result, which = "signed_surprise")
#'
#' # Custom palette
#' plot(result, pal = heat.colors(9))
plot.bs_surprise_sf <- function(x, y = NULL,
                                 which = c("surprise", "signed_surprise"),
                                 pal = NULL,
                                 breaks = NULL,
                                 nbreaks = 9,
                                 main = NULL,
                                 border = "grey50",
                                 lwd = 0.2,
                                 key.pos = 4,
                                 ...) {
  which <- match.arg(which)

  # Check if the requested column exists
  if (which == "signed_surprise" && !"signed_surprise" %in% names(x)) {
    cli_warn("signed_surprise not available, using surprise instead.")
    which <- "surprise"
  }

  # Get values for color scaling
  vals <- x[[which]]

  # Set default palette
  if (is.null(pal)) {
    if (which == "signed_surprise") {
      # Diverging palette for signed
      pal <- diverging_palette(nbreaks)
    } else {
      # Sequential palette for unsigned
      pal <- sequential_palette(nbreaks)
    }
  }

  # Set breaks
  if (is.null(breaks)) {
    if (which == "signed_surprise") {
      # Symmetric breaks for signed surprise
      max_abs <- max(abs(vals), na.rm = TRUE)
      breaks <- seq(-max_abs, max_abs, length.out = nbreaks + 1)
    } else {
      # For unsigned surprise, create explicit breaks to match palette length
      breaks <- seq(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE), length.out = nbreaks + 1)
    }
  }

  # Set title
  if (is.null(main)) {
    main <- if (which == "signed_surprise") {
      "Signed Bayesian Surprise"
    } else {
      "Bayesian Surprise"
    }
  }

  # Plot using sf (call the base graphics plot, which dispatches to sf method)
  graphics::plot(
    x[which],
    main = main,
    pal = pal,
    breaks = breaks,
    border = border,
    lwd = lwd,
    key.pos = key.pos,
    ...
  )

  invisible(x)
}

#' Plot Surprise Result
#'
#' Creates a simple histogram or density plot of surprise values.
#'
#' @param x A `bs_surprise` object
#' @param y Unused
#' @param which Which to plot: "surprise" or "signed_surprise"
#' @param type Plot type: "histogram" or "density"
#' @param main Plot title
#' @param xlab X-axis label
#' @param col Fill color
#' @param border Border color
#' @param ... Additional arguments passed to [hist()] or [density()]
#'
#' @return Invisibly returns the input object
#'
#' @export
plot.bs_surprise <- function(x, y = NULL,
                              which = c("surprise", "signed_surprise"),
                              type = c("histogram", "density"),
                              main = NULL,
                              xlab = NULL,
                              col = "#4575b4",
                              border = "white",
                              ...) {
  which <- match.arg(which)
  type <- match.arg(type)

  vals <- get_surprise(x, which)

  if (is.null(main)) {
    main <- if (which == "signed_surprise") {
      "Distribution of Signed Surprise"
    } else {
      "Distribution of Surprise"
    }
  }

  if (is.null(xlab)) {
    xlab <- if (which == "signed_surprise") "Signed Surprise" else "Surprise"
  }

  if (type == "histogram") {
    graphics::hist(vals, main = main, xlab = xlab, col = col, border = border, ...)
  } else {
    dens <- stats::density(vals, na.rm = TRUE)
    graphics::plot(dens, main = main, xlab = xlab, ...)
    graphics::polygon(dens, col = col, border = border)
  }

  invisible(x)
}

#' Plot Temporal Surprise
#'
#' Creates time series plots of surprise evolution.
#'
#' @param x A `bs_surprise_temporal` object
#' @param y Unused
#' @param type Plot type: "time_series", "heatmap", or "cumulative"
#' @param regions Which regions to plot (for time_series). If NULL, plots all.
#' @param main Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param col Colors
#' @param ... Additional arguments
#'
#' @return Invisibly returns the input object
#'
#' @export
plot.bs_surprise_temporal <- function(x, y = NULL,
                                       type = c("time_series", "heatmap", "cumulative"),
                                       regions = NULL,
                                       main = NULL,
                                       xlab = "Time",
                                       ylab = NULL,
                                       col = NULL,
                                       ...) {
  type <- match.arg(type)

  times <- x$time_values
  surprise_mat <- x$cumulative_surprise

  if (is.null(main)) {
    main <- switch(type,
      time_series = "Surprise Over Time",
      heatmap = "Surprise Heatmap",
      cumulative = "Cumulative Surprise"
    )
  }

  if (is.null(ylab)) {
    ylab <- switch(type,
      time_series = "Surprise",
      heatmap = "Region",
      cumulative = "Cumulative Surprise"
    )
  }

  if (type == "time_series") {
    # Filter regions if specified
    if (!is.null(regions)) {
      region_idx <- which(colnames(surprise_mat) %in% as.character(regions))
      surprise_mat <- surprise_mat[, region_idx, drop = FALSE]
    }

    n_regions <- ncol(surprise_mat)

    if (is.null(col)) {
      col <- grDevices::rainbow(n_regions)
    }

    # Initialize plot
    ylim <- range(surprise_mat, na.rm = TRUE)
    graphics::plot(times, surprise_mat[, 1], type = "l", col = col[1],
                   main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)

    # Add lines for other regions
    if (n_regions > 1) {
      for (i in 2:n_regions) {
        graphics::lines(times, surprise_mat[, i], col = col[i])
      }
      graphics::legend("topright", legend = colnames(surprise_mat),
                       col = col, lty = 1, cex = 0.7)
    }

  } else if (type == "heatmap") {
    # Heatmap of surprise over time and regions
    if (is.null(col)) {
      col <- grDevices::hcl.colors(20, "YlOrRd", rev = TRUE)
    }
    graphics::image(times, seq_len(ncol(surprise_mat)), surprise_mat,
                    main = main, xlab = xlab, ylab = ylab, col = col, ...)
    graphics::axis(2, at = seq_len(ncol(surprise_mat)),
                   labels = colnames(surprise_mat), las = 1, cex.axis = 0.7)

  } else if (type == "cumulative") {
    # Cumulative surprise over time
    cum_surprise <- apply(surprise_mat, 1, sum, na.rm = TRUE)
    cum_total <- cumsum(cum_surprise)

    if (is.null(col)) col <- "#4575b4"

    graphics::plot(times, cum_total, type = "l", col = col, lwd = 2,
                   main = main, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' Plot Model Space
#'
#' Visualizes the prior and posterior probabilities of a model space.
#'
#' @param x A `bs_model_space` object
#' @param y Unused
#' @param main Plot title
#' @param col Colors for prior and posterior bars
#' @param ... Additional arguments passed to [barplot()]
#'
#' @return Invisibly returns the input object
#'
#' @export
plot.bs_model_space <- function(x, y = NULL,
                                 main = "Model Probabilities",
                                 col = c("#4575b4", "#d73027"),
                                 ...) {
  n_models <- x$n_models
  model_names <- names(x$models)

  # Prepare data matrix
  if (!is.null(x$posterior)) {
    probs <- rbind(x$prior, x$posterior)
    rownames(probs) <- c("Prior", "Posterior")
    legend_text <- c("Prior", "Posterior")
  } else {
    probs <- matrix(x$prior, nrow = 1)
    rownames(probs) <- "Prior"
    legend_text <- "Prior"
    col <- col[1]
  }
  colnames(probs) <- model_names

  # Create barplot
  graphics::barplot(probs, beside = TRUE, main = main, col = col,
                    ylim = c(0, 1), ylab = "Probability",
                    las = 2, ...)
  graphics::legend("topright", legend = legend_text, fill = col)

  invisible(x)
}

# Helper Functions for Palettes ------------------------------------------------

#' Generate Sequential Color Palette
#' @noRd
sequential_palette <- function(n = 9) {
  grDevices::hcl.colors(n, "YlOrRd", rev = TRUE)
}

#' Generate Diverging Color Palette
#' @noRd
diverging_palette <- function(n = 9) {
  grDevices::hcl.colors(n, "RdBu", rev = TRUE)
}

#' Generate Qualitative Color Palette
#' @noRd
qualitative_palette <- function(n = 8) {
  grDevices::hcl.colors(n, "Dark 3")
}
