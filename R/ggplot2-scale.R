# ggplot2 Scales for Bayesian Surprise
#
# Custom color scales for surprise maps.

#' Surprise Color Scale (Sequential)
#'
#' Sequential color scale for unsigned surprise values. Uses viridis palettes
#' for perceptually uniform color mapping.
#'
#' @param ... Arguments passed to [ggplot2::scale_fill_viridis_c()]
#' @param option Viridis palette option: "magma" (A), "inferno" (B),
#'   "plasma" (C), "viridis" (D), "cividis" (E), "rocket" (F),
#'   "mako" (G), "turbo" (H)
#' @param direction Direction of palette (1 = low-to-high, -1 = reversed)
#' @param name Legend title
#' @param begin,end Range of palette to use (0 to 1)
#'
#' @return A ggplot2 scale object
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Basic usage - geometry must be mapped explicitly
#' ggplot(nc) +
#'   geom_surprise(aes(geometry = geometry, observed = SID74, expected = BIR74)) +
#'   scale_fill_surprise()
scale_fill_surprise <- function(..., option = "inferno", direction = 1,
                                 name = "Surprise", begin = 0, end = 1) {
  ggplot2::scale_fill_viridis_c(
    ...,
    option = option,
    direction = direction,
    name = name,
    begin = begin,
    end = end
  )
}

#' @rdname scale_fill_surprise
#' @export
scale_colour_surprise <- function(..., option = "inferno", direction = 1,
                                   name = "Surprise", begin = 0, end = 1) {
  ggplot2::scale_colour_viridis_c(
    ...,
    option = option,
    direction = direction,
    name = name,
    begin = begin,
    end = end
  )
}

#' @rdname scale_fill_surprise
#' @export
scale_color_surprise <- scale_colour_surprise

#' Signed Surprise Color Scale (Diverging)
#'
#' Diverging color scale for signed surprise values. Negative values
#' (lower than expected) are shown in one color, positive values (higher
#' than expected) in another, with neutral (zero) in the middle.
#'
#' @param low Color for negative surprise (lower than expected).
#'   Default is blue.
#' @param mid Color for zero surprise (as expected). Default is white.
#' @param high Color for positive surprise (higher than expected).
#'   Default is red.
#' @param midpoint Value of the midpoint. Default is 0.
#' @param name Legend title
#' @param ... Additional arguments passed to [ggplot2::scale_fill_gradient2()]
#'
#' @return A ggplot2 scale object
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Signed surprise with diverging scale - geometry must be mapped explicitly
#' ggplot(nc) +
#'   geom_surprise(
#'     aes(geometry = geometry, observed = SID74, expected = BIR74),
#'     fill_type = "signed"
#'   ) +
#'   scale_fill_surprise_diverging()
scale_fill_surprise_diverging <- function(low = scales::muted("blue"),
                                           mid = "white",
                                           high = scales::muted("red"),
                                           midpoint = 0,
                                           name = "Signed Surprise",
                                           ...) {
  ggplot2::scale_fill_gradient2(
    low = low,
    mid = mid,
    high = high,
    midpoint = midpoint,
    name = name,
    ...
  )
}

#' @rdname scale_fill_surprise_diverging
#' @export
scale_colour_surprise_diverging <- function(low = scales::muted("blue"),
                                             mid = "white",
                                             high = scales::muted("red"),
                                             midpoint = 0,
                                             name = "Signed Surprise",
                                             ...) {
  ggplot2::scale_colour_gradient2(
    low = low,
    mid = mid,
    high = high,
    midpoint = midpoint,
    name = name,
    ...
  )
}

#' @rdname scale_fill_surprise_diverging
#' @export
scale_color_surprise_diverging <- scale_colour_surprise_diverging

#' Binned Surprise Scale
#'
#' Discrete binned color scale for surprise values. Useful for creating
#' choropleth maps with clearly distinguishable categories.
#'
#' @param n.breaks Number of breaks/bins
#' @param palette ColorBrewer palette name. For unsigned surprise,
#'   sequential palettes like "YlOrRd", "Oranges", "Reds" work well.
#' @param direction Direction of palette (1 = normal, -1 = reversed)
#' @param name Legend title
#' @param ... Additional arguments passed to [ggplot2::scale_fill_fermenter()]
#'
#' @return A ggplot2 scale object
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Binned surprise scale - geometry must be mapped explicitly
#' ggplot(nc) +
#'   geom_surprise(aes(geometry = geometry, observed = SID74, expected = BIR74)) +
#'   scale_fill_surprise_binned(n.breaks = 5)
scale_fill_surprise_binned <- function(n.breaks = 5,
                                        palette = "YlOrRd",
                                        direction = 1,
                                        name = "Surprise",
                                        ...) {
  ggplot2::scale_fill_fermenter(
    palette = palette,
    direction = direction,
    n.breaks = n.breaks,
    name = name,
    ...
  )
}

#' @rdname scale_fill_surprise_binned
#' @export
scale_colour_surprise_binned <- function(n.breaks = 5,
                                          palette = "YlOrRd",
                                          direction = 1,
                                          name = "Surprise",
                                          ...) {
  ggplot2::scale_colour_fermenter(
    palette = palette,
    direction = direction,
    n.breaks = n.breaks,
    name = name,
    ...
  )
}

#' @rdname scale_fill_surprise_binned
#' @export
scale_color_surprise_binned <- scale_colour_surprise_binned

#' Binned Diverging Surprise Scale
#'
#' Binned diverging scale for signed surprise values.
#'
#' @inheritParams scale_fill_surprise_diverging
#' @param n.breaks Number of breaks/bins
#' @param palette ColorBrewer diverging palette name.
#'   Options: "RdBu", "RdYlBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdGy", "Spectral"
#' @param direction Direction of palette (1 = normal, -1 = reversed)
#' @param ... Additional arguments
#'
#' @return A ggplot2 scale object
#'
#' @export
scale_fill_surprise_diverging_binned <- function(n.breaks = 7,
                                                   palette = "RdBu",
                                                   direction = -1,
                                                   name = "Signed Surprise",
                                                   ...) {
  ggplot2::scale_fill_fermenter(
    palette = palette,
    direction = direction,
    n.breaks = n.breaks,
    name = name,
    ...
  )
}

#' Manual Surprise Breaks Scale
#'
#' Create a scale with manually specified breaks for surprise values.
#'
#' @param breaks Numeric vector of break points
#' @param colors Character vector of colors (length should be length(breaks) + 1
#'   for binned, or length(breaks) for continuous)
#' @param name Legend title
#' @param labels Labels for legend
#' @param na.value Color for NA values
#' @param ... Additional arguments
#'
#' @return A ggplot2 scale object
#'
#' @export
scale_fill_surprise_manual <- function(breaks = c(0.5, 1, 1.5, 2),
                                        colors = c("#ffffb2", "#fecc5c", "#fd8d3c",
                                                   "#f03b20", "#bd0026"),
                                        name = "Surprise",
                                        labels = waiver(),
                                        na.value = "grey50",
                                        ...) {
  ggplot2::scale_fill_stepsn(
    colors = colors,
    breaks = breaks,
    name = name,
    labels = labels,
    na.value = na.value,
    ...
  )
}
