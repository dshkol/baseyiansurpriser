# Tests for ggplot2 integration

test_that("StatSurprise computes surprise correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  # Create a basic ggplot with stat_surprise - must map geometry explicitly
  p <- ggplot2::ggplot(nc) +
    stat_surprise(ggplot2::aes(geometry = geometry, observed = SID74, expected = BIR74))

  # Build the plot to trigger stat computation
  built <- ggplot2::ggplot_build(p)

  expect_true("fill" %in% names(built$data[[1]]))
})

test_that("geom_surprise creates valid layer", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  # Must map geometry explicitly for sf data
  p <- ggplot2::ggplot(nc) +
    geom_surprise(ggplot2::aes(geometry = geometry, observed = SID74, expected = BIR74))

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_surprise works with fill_type options", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  # Must map geometry explicitly for sf data
  p_surprise <- ggplot2::ggplot(nc) +
    geom_surprise(ggplot2::aes(geometry = geometry, observed = SID74, expected = BIR74),
                  fill_type = "surprise")

  p_signed <- ggplot2::ggplot(nc) +
    geom_surprise(ggplot2::aes(geometry = geometry, observed = SID74, expected = BIR74),
                  fill_type = "signed")

  expect_s3_class(p_surprise, "ggplot")
  expect_s3_class(p_signed, "ggplot")
})

test_that("scale_fill_surprise returns valid scale", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  s <- scale_fill_surprise()
  expect_s3_class(s, "Scale")
  expect_s3_class(s, "ScaleContinuous")
})
test_that("scale_fill_surprise_diverging returns valid scale", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  s <- scale_fill_surprise_diverging()
  expect_s3_class(s, "Scale")
  expect_s3_class(s, "ScaleContinuous")
})

test_that("scale_fill_surprise_binned returns valid scale", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  s <- scale_fill_surprise_binned()
  expect_s3_class(s, "Scale")
})

test_that("full ggplot2 workflow executes without error", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  # Full workflow - must map geometry explicitly
  p <- ggplot2::ggplot(nc) +
    geom_surprise(ggplot2::aes(geometry = geometry, observed = SID74, expected = BIR74)) +
    scale_fill_surprise() +
    ggplot2::labs(title = "Test Plot")

  # Should be able to build without error
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("ggplot2 with signed surprise and diverging scale works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  # Must map geometry explicitly
  p <- ggplot2::ggplot(nc) +
    geom_surprise(ggplot2::aes(geometry = geometry, observed = SID74, expected = BIR74),
                  fill_type = "signed") +
    scale_fill_surprise_diverging()

  expect_silent(ggplot2::ggplot_build(p))
})
