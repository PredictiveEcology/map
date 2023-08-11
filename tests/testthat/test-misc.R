test_that(".rasterToMemory handles Raster and SpatRaster objects", {
  ## raster
  f <- system.file("external/test.grd", package = "raster")
  r <- raster::raster(f)
  r2 <- .rasterToMemory(r)
  expect_true(raster::inMemory(r2))

  ## terra
  f <- system.file("ex/test.grd", package = "terra")
  r <- terra::rast(f)
  r2 <- .rasterToMemory(r)
  expect_true(terra::inMemory(r2))
  expect_true(terra::hasValues(r2))
})

test_that("gdal_polygonizer rewrite works as previously", {
  skip_if_not_installed("gdalUtils")
  skip_on_cran()

  ## dummy example
  r <- terra::rast(ncols = 20, nrows = 20)
  terra::values(r) <- sample(1:10, terra::ncell(r), replace = TRUE)

  p <- gdal_polygonizeR(r)
  p.old <- map:::.gdal_polygonizeR(raster(r)) |> terra::vect()

  expect_equal(p, p.old)

  ## raster
  f <- system.file("external/vegTypeMap.grd", package = "map")
  r <- raster::raster(f)
  p <- gdal_polygonizeR(r)
  p.old <- map:::.gdal_polygonizeR(r) |> terra::vect()
  expect_equal(p, p.old)

  ## terra
  f <- system.file("external/vegTypeMap.grd", package = "map")
  r <- terra::rast(f)
  p <- gdal_polygonizeR(r)
  p.old <- map:::.gdal_polygonizeR(r) |> terra::vect()
  expect_equal(p, p.old)
})
