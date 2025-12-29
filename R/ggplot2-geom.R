# ggplot2 Geom for Bayesian Surprise
#
# Convenience wrapper combining stat_surprise with geom_sf.

#' Surprise Map Geom
#'
#' A convenience geom that combines `stat_surprise` with `geom_sf` for
#' easy creation of surprise maps.
#'
#' @inheritParams stat_surprise
#' @param fill_type Type of surprise for fill aesthetic:
#'   - "surprise": Unsigned surprise (always positive)
#'   - "signed": Signed surprise (positive = higher than expected,
#'     negative = lower than expected)
#' @param color,colour Border color for polygons
#' @param linewidth Border line width
#'
#' @return A list of ggplot2 layers
#'
#' @section Aesthetics:
#' `geom_surprise` understands the following aesthetics:
#' \describe{
#'   \item{geometry}{sf geometry column (required)}
#'   \item{observed}{Observed values (required)}
#'   \item{expected}{Expected values (optional, for base rate model)}
#'   \item{sample_size}{Sample sizes (optional, for funnel model)}
#'   \item{fill}{Mapped to surprise by default}
#'   \item{color/colour}{Border color}
#' }
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Basic surprise map - geometry must be mapped explicitly
#' ggplot(nc) +
#'   geom_surprise(aes(geometry = geometry, observed = SID74, expected = BIR74)) +
#'   scale_fill_surprise()
#'
#' # Signed surprise with diverging scale
#' ggplot(nc) +
#'   geom_surprise(
#'     aes(geometry = geometry, observed = SID74, expected = BIR74),
#'     fill_type = "signed"
#'   ) +
#'   scale_fill_surprise_diverging()
#'
#' # Customize appearance
#' ggplot(nc) +
#'   geom_surprise(
#'     aes(geometry = geometry, observed = SID74, expected = BIR74),
#'     color = "white",
#'     linewidth = 0.2
#'   ) +
#'   scale_fill_surprise() +
#'   theme_minimal()
geom_surprise <- function(mapping = NULL, data = NULL,
                          position = "identity",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          fill_type = c("surprise", "signed"),
                          models = c("uniform", "baserate", "funnel"),
                          color = NA, colour = color, linewidth = 0.1,
                          ...) {
  fill_type <- match.arg(fill_type)
  signed <- fill_type == "signed"

  # Set default fill mapping based on fill_type
  surprise_col <- if (signed) "signed_surprise" else "surprise"

  # Build mapping
  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  # Add fill aesthetic if not already specified
  if (!"fill" %in% names(mapping)) {
    fill_mapping <- ggplot2::aes(fill = ggplot2::after_stat(!!rlang::sym(surprise_col)))
    mapping <- modifyList(mapping, fill_mapping)
  }

  c(
    ggplot2::layer(
      geom = ggplot2::GeomSf,
      stat = StatSurprise,
      data = data,
      mapping = mapping,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        models = models,
        signed = signed,
        colour = colour,
        linewidth = linewidth,
        ...
      )
    ),
    ggplot2::coord_sf(default = TRUE)
  )
}

#' Surprise Histogram
#'
#' Creates a histogram of surprise values.
#'
#' @param mapping Aesthetic mapping
#' @param data Data with surprise values
#' @param which Which surprise to plot: "surprise" or "signed_surprise"
#' @param bins Number of bins
#' @param fill Fill color
#' @param color Border color
#' @param ... Additional arguments passed to [ggplot2::geom_histogram()]
#'
#' @return A ggplot2 layer
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc_surprise <- surprise(nc, observed = SID74, expected = BIR74)
#'
#' ggplot(nc_surprise) +
#'   geom_surprise_histogram()
geom_surprise_histogram <- function(mapping = NULL, data = NULL,
                                     which = c("surprise", "signed_surprise"),
                                     bins = 30, fill = "#4575b4", color = "white",
                                     ...) {
  which <- match.arg(which)

  if (is.null(mapping)) {
    mapping <- ggplot2::aes(x = .data[[which]])
  }

  ggplot2::geom_histogram(
    mapping = mapping,
    data = data,
    bins = bins,
    fill = fill,
    color = color,
    ...
  )
}

#' Surprise Density Plot
#'
#' Creates a density plot of surprise values.
#'
#' @inheritParams geom_surprise_histogram
#'
#' @return A ggplot2 layer
#'
#' @export
geom_surprise_density <- function(mapping = NULL, data = NULL,
                                   which = c("surprise", "signed_surprise"),
                                   fill = "#4575b4", color = "black",
                                   ...) {
  which <- match.arg(which)

  if (is.null(mapping)) {
    mapping <- ggplot2::aes(x = .data[[which]])
  }

  ggplot2::geom_density(
    mapping = mapping,
    data = data,
    fill = fill,
    color = color,
    alpha = 0.7,
    ...
  )
}
