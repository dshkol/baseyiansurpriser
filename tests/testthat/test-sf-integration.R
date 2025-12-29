# Tests for sf integration

test_that("surprise.sf works with sf objects", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  result <- surprise(nc, observed = SID74, expected = BIR74)

  expect_s3_class(result, "bs_surprise_sf")
  expect_s3_class(result, "sf")
  expect_true("surprise" %in% names(result))
  expect_true("signed_surprise" %in% names(result))
})

test_that("st_surprise is equivalent to surprise.sf", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  result1 <- surprise(nc, observed = SID74, expected = BIR74)
  result2 <- st_surprise(nc, observed = SID74, expected = BIR74)

  expect_equal(result1$surprise, result2$surprise)
})

test_that("surprise_result attribute is attached", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  result <- surprise(nc, observed = SID74, expected = BIR74)

  sr <- attr(result, "surprise_result")
  expect_s3_class(sr, "bs_surprise")
  expect_false(is.null(sr$model_space))
})

test_that("get_surprise extracts values correctly", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  result <- surprise(nc, observed = SID74, expected = BIR74)

  surprise_vals <- get_surprise(result, "surprise")
  signed_vals <- get_surprise(result, "signed")

  expect_equal(length(surprise_vals), nrow(nc))
  expect_equal(length(signed_vals), nrow(nc))
})

test_that("get_model_space extracts model space", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  result <- surprise(nc, observed = SID74, expected = BIR74)

  mspace <- get_model_space(result)
  expect_s3_class(mspace, "bs_model_space")
})
