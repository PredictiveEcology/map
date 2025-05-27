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

test_that("addColumnNameForLabels handles multiple object classes", {
  skip_if_not_installed("SpaDES.tools")

  p <- SpaDES.tools::randomPolygons() |>
    terra::as.polygons()

  ## terra
  pt <- p
  pt1 <- addColumnNameForLabels(pt, 1)
  pt2 <- addColumnNameForLabels(pt, "lyr.1")
  expect_equal(names(pt1), c("lyr.1", "shinyLabel"))
  expect_equal(names(pt2), c("lyr.1", "shinyLabel"))

  ## sf
  pf <- sf::st_as_sf(p)
  pf1 <- addColumnNameForLabels(pf, 1)
  pf2 <- addColumnNameForLabels(pf, "lyr.1")
  expect_equal(names(pf1), c("lyr.1", "geometry", "shinyLabel"))
  expect_equal(names(pf2), c("lyr.1", "geometry", "shinyLabel"))

  ## sp
  ps <- sf::st_as_sf(p) |> sf::as_Spatial()
  ps1 <- addColumnNameForLabels(ps, 1)
  ps2 <- addColumnNameForLabels(ps, "lyr.1")
  expect_equal(names(ps1), c("lyr.1", "shinyLabel"))
  expect_equal(names(ps2), c("lyr.1", "shinyLabel"))

  ## list
  pl <- list(pt, pf, ps)
  pl1 <- addColumnNameForLabels(pl, 1)
  pl2 <- addColumnNameForLabels(pl, "lyr.1")
  expect_true(is.list(pl1))
  expect_true(all.equal(pl1[[1]], pt1)) ## use terra::all.equal
  expect_equal(pl1[[2]], pf1)
  expect_equal(pl1[[3]], ps1)

  expect_true(is.list(pl2))
  expect_true(all.equal(pl2[[1]], pt2)) ## use terra::all.equal
  expect_equal(pl2[[2]], pf2)
  expect_equal(pl2[[3]], ps2)
})

test_that("gdal_polygonizer rewrite works as previously", {
  skip_on_cran()

  ## dummy example
  r <- terra::rast(ncols = 20, nrows = 20)
  terra::values(r) <- sample(1:10, terra::ncell(r), replace = TRUE)

  p <- gdal_polygonizeR(r)
  p.old <- map:::.gdal_polygonizeR(raster(r)) |> terra::vect()

  expect_true(all.equal(p, p.old)) ## use terra::all.equal

  ## raster
  f <- system.file("external/vegTypeMap.grd", package = "map")
  r <- raster::raster(f)
  p <- gdal_polygonizeR(r)
  p.old <- map:::.gdal_polygonizeR(r) |> terra::vect()
  expect_true(all.equal(p, p.old)) ## use terra::all.equal

  ## terra
  f <- system.file("external/vegTypeMap.grd", package = "map")
  r <- terra::rast(f)
  p <- gdal_polygonizeR(r)
  p.old <- map:::.gdal_polygonizeR(r) |> terra::vect()
  expect_true(all.equal(p, p.old)) ## use terra::all.equal
})
