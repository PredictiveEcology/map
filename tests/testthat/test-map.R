test_that("mapAdd doesn't work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("SpaDES.tools")

  ## TODO: `LargePatches` and `LeadingVegTypeByAgeClass` were moved to `LandWebUtils`,
  ##  which is a reverse dependency of this package, so it can't be used here.

  withr::local_package("terra")

  withr::local_tempdir("test_map_") |>
    withr::local_dir()

  coords <- matrix(c(
    -122.98, -116.1,
    -99.2, -106,
    -122.98, 59.9,
    65.73, 63.58,
    54.79, 59.9
  ), ncol = 2)

  center <- sf::st_multipoint(coords) |>
    sf::st_centroid() |>
    as.matrix()

  studyArea <- SpaDES.tools::randomPolygon(center, area = 1e5) |>
    sf::st_as_sf(crs = "epsg:4326")
  studyArea[["ID"]] <- 1L
  studyArea[["shinyLabel"]] <- "zone2"
  studyArea <- sf::as_Spatial(studyArea)

  ml <- mapAdd(studyArea,
    isStudyArea = TRUE, layerName = "Small Study Area",
    poly = TRUE, analysisGroup2 = "Small Study Area"
  )

  ## add second study area within the first
  smallStudyArea <- SpaDES.tools::randomPolygon(center, area = 1e4) |>
    sf::st_as_sf(crs = "epsg:4326")
  smallStudyArea[["ID"]] <- 1L
  smallStudyArea[["shinyLabel"]] <- "zone1"
  smallStudyArea <- sf::as_Spatial(smallStudyArea)

  ml <- mapAdd(smallStudyArea, ml,
    isStudyArea = TRUE, filename2 = NULL,
    analysisGroup2 = "Smaller Study Area",
    poly = TRUE,
    layerName = "Smaller Study Area"
  )

  rasTemplate <- terra::rast(terra::ext(studyArea(ml)), resolution = 1e-5, crs = "epsg:4326")
  tsf <- SpaDES.tools::randomPolygons(rasTemplate, numTypes = 8) * 30
  vtm <- SpaDES.tools::randomPolygons(tsf, numTypes = 4)
  vtm <- terra::as.factor(vtm)
  levels(vtm) <- data.frame(
    ID = sort(unique(vtm[])),
    VALUE = c("black spruce", "white spruce", "aspen", "fir")
  )

  ## need python + gdal to generate leaflet tiles
  doLeaflet <- canMakeTiles()

  ml <- mapAdd(tsf, ml,
    filename2 = "tsf1.tif", layerName = "tsf1",
    tsf = "tsf1.tif",
    analysisGroup1 = "tsf1_vtm1", leaflet = doLeaflet, overwrite = TRUE
  )
  ml <- mapAdd(vtm, ml,
    filename2 = "vtm1.grd", layerName = "vtm1",
    vtm = "vtm1.grd",
    analysisGroup1 = "tsf1_vtm1", leaflet = doLeaflet, overwrite = TRUE
  )

  expect_true(all(
    c("Small Study Area", "Small Study Area", "tsf1", "vtm1") %in% metadata(ml)[["layerName"]]
  ))

  ## -----------------------------------------------------------------------------------------------
  skip("need LandWebUtils")
  skip_if_not_installed("sp")
  ## TODO: `LargePatches` and `LeadingVegTypeByAgeClass` were moved to `LandWebUtils`,
  ##  which is a reverse dependency of this package, so it can't be used here.

  ageClasses <- c("Young", "Immature", "Mature", "Old")
  ageClassCutOffs <- c(0, 40, 80, 120)

  # add an analysis -- this will trigger analyses because there are already objects in the map
  #    This will trigger 2 analyses ... LeadingVegTypeByAgeClass on each raster x polygon combo
  #    (only 1 currently)
  #    so there is 1 raster group, 2 polygon groups, 1 analyses - Total 2, 2 run now
  ml <- mapAddAnalysis(ml,
    functionName = "LeadingVegTypeByAgeClass",
    ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs
  )
  # add an analysis -- this will trigger analyses because there are already objects in the map
  #    This will trigger 2 more analyses ... largePatches on each raster x polygon combo
  #    (only 1 currently)
  #    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
  ml <- mapAddAnalysis(ml,
    functionName = "LargePatches", ageClasses = ageClasses,
    id = "1", labelColumn = "shinyLabel",
    ageClassCutOffs = ageClassCutOffs
  )

  # Add a second polygon, trigger
  smallStudyArea2 <- SpaDES.tools::randomPolygon(studyArea(ml), 1e5)
  smallStudyArea2 <- sp::SpatialPolygonsDataFrame(smallStudyArea2,
    data = data.frame(ID = 1, shinyLabel = "zone1"),
    match.ID = FALSE
  )
  # add a new layer -- this will trigger analyses because there are already analyese in the map
  #    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
  #    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
  ml <- mapAdd(smallStudyArea2, ml,
    isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
    analysisGroup2 = "Smaller Study Area 2",
    poly = TRUE,
    layerName = "Smaller Study Area 2"
  ) # adds a second studyArea within 1st

  # Add a *different* second polygon, via overwrite. This should trigger new analyses
  smallStudyArea2 <- SpaDES.tools::randomPolygon(studyArea(ml), 1e5)
  smallStudyArea2 <- sp::SpatialPolygonsDataFrame(smallStudyArea2,
    data = data.frame(ID = 1, shinyLabel = "zone1"),
    match.ID = FALSE
  )
  # add a new layer -- this will trigger analyses because there are already analyese in the map
  #    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
  #    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
  ml <- mapAdd(smallStudyArea2, ml,
    isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
    analysisGroup2 = "Smaller Study Area 2",
    poly = TRUE,
    layerName = "Smaller Study Area 2"
  ) # adds a second studyArea within 1st

  # Add a 2nd pair of rasters
  rasTemplate <- raster::raster(raster::extent(studyArea(ml)), res = 0.001)
  tsf2 <- SpaDES.tools::randomPolygons(rasTemplate, numTypes = 8) * 30
  raster::crs(tsf2) <- raster::crs(ml)
  vtm2 <- SpaDES.tools::randomPolygons(tsf2, numTypes = 4)
  levels(vtm2) <- data.frame(
    ID = sort(unique(vtm2[])),
    Factor = c("black spruce", "white spruce", "aspen", "fir")
  )
  raster::crs(vtm2) <- raster::crs(ml)
  ml <- mapAdd(tsf2, ml,
    filename2 = "tsf2.tif", layerName = "tsf2",
    tsf = "tsf2.tif",
    analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE
  )
  ml <- mapAdd(vtm2, ml,
    filename2 = "vtm2.grd", layerName = "vtm2",
    vtm = "vtm2.grd",
    analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE
  )

  # post hoc analysis of data
  #  use or create a specialized function that can handle the analysesData slot
  ml <- mapAddPostHocAnalysis(
    map = ml, functionName = "rbindlistAG",
    postHocAnalysisGroups = "analysisGroup2",
    postHocAnalyses = "all"
  )
})
