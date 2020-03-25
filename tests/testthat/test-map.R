test_that("mapAdd doesn't work", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  testInitOut <- testInit(c("raster", "sp", "reproducible", "SpaDES.tools"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  setwd(tempdir())
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  sr1 <- Polygon(coords)
  srs1 <- Polygons(list(sr1), "s1")
  studyArea <- SpatialPolygons(list(srs1), 1L)
  crs(studyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  studyArea <- SpatialPolygonsDataFrame(studyArea,
                                        data = data.frame(ID = 1, shinyLabel = "zone2"),
                                        match.ID = FALSE)

  ml <- mapAdd(studyArea, isStudyArea = TRUE, layerName = "Small Study Area",
               poly = TRUE, analysisGroup2 = "Small Study Area")

  # if (require("SpaDES.tools")) {
  #require()
  smallStudyArea <- randomPolygon(studyArea(ml), 1e5)
  smallStudyArea <- SpatialPolygonsDataFrame(smallStudyArea,
                                             data = data.frame(ID = 1, shinyLabel = "zone1"),
                                             match.ID = FALSE)
  ml <- mapAdd(smallStudyArea, ml, isStudyArea = TRUE, filename2 = NULL,
               analysisGroup2 = "Smaller Study Area",
               poly = TRUE,
               layerName = "Smaller Study Area") # adds a second studyArea within 1st

  rasTemplate <- raster(extent(studyArea(ml)), res = 0.001)
  tsf <- randomPolygons(rasTemplate, numTypes = 8) * 30
  crs(tsf) <- crs(ml)
  vtm <- randomPolygons(tsf, numTypes = 4)
  levels(vtm) <- data.frame(ID = sort(unique(vtm[])),
                            Factor = c("black spruce", "white spruce", "aspen", "fir"))
  crs(vtm) <- crs(ml)
  ml <- mapAdd(tsf, ml, filename2 = "tsf1.tif", layerName = "tsf1",
               tsf = "tsf1.tif",
               analysisGroup1 = "tsf1_vtm1", leaflet = TRUE, overwrite = TRUE)
  ml <- mapAdd(vtm, ml, filename2 = "vtm1.grd", layerName = "vtm1",
               vtm = "vtm1.grd",
               analysisGroup1 = "tsf1_vtm1", leaflet = TRUE, overwrite = TRUE)

  ageClasses <- c("Young", "Immature", "Mature", "Old")
  ageClassCutOffs <- c(0, 40, 80, 120)

  # add an analysis -- this will trigger analyses because there are already objects in the map
  #    This will trigger 2 analyses ... LeadingVegTypeByAgeClass on each raster x polygon combo
  #    (only 1 currently)
  #    so there is 1 raster group, 2 polygon groups, 1 analyses - Total 2, 2 run now
  ml <- mapAddAnalysis(ml, functionName = "LeadingVegTypeByAgeClass",
                       ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)
  # add an analysis -- this will trigger analyses because there are already objects in the map
  #    This will trigger 2 more analyses ... largePatches on each raster x polygon combo
  #    (only 1 currently)
  #    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
  ml <- mapAddAnalysis(ml, functionName = "LargePatches", ageClasses = ageClasses,
                       id = "1", labelColumn = "shinyLabel",
                       ageClassCutOffs = ageClassCutOffs)

  # Add a second polygon, trigger
  smallStudyArea2 <- randomPolygon(studyArea(ml), 1e5)
  smallStudyArea2 <- SpatialPolygonsDataFrame(smallStudyArea2,
                                              data = data.frame(ID = 1, shinyLabel = "zone1"),
                                              match.ID = FALSE)
  # add a new layer -- this will trigger analyses because there are already analyese in the map
  #    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
  #    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
  ml <- mapAdd(smallStudyArea2, ml, isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
               analysisGroup2 = "Smaller Study Area 2",
               poly = TRUE,
               layerName = "Smaller Study Area 2") # adds a second studyArea within 1st

  # Add a *different* second polygon, via overwrite. This should trigger new analyses
  smallStudyArea2 <- randomPolygon(studyArea(ml), 1e5)
  smallStudyArea2 <- SpatialPolygonsDataFrame(smallStudyArea2,
                                              data = data.frame(ID = 1, shinyLabel = "zone1"),
                                              match.ID = FALSE)
  # add a new layer -- this will trigger analyses because there are already analyese in the map
  #    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
  #    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
  ml <- mapAdd(smallStudyArea2, ml, isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
               analysisGroup2 = "Smaller Study Area 2",
               poly = TRUE,
               layerName = "Smaller Study Area 2") # adds a second studyArea within 1st

  # Add a 2nd pair of rasters
  rasTemplate <- raster(extent(studyArea(ml)), res = 0.001)
  tsf2 <- randomPolygons(rasTemplate, numTypes = 8) * 30
  crs(tsf2) <- crs(ml)
  vtm2 <- randomPolygons(tsf2, numTypes = 4)
  levels(vtm2) <- data.frame(ID = sort(unique(vtm2[])),
                             Factor = c("black spruce", "white spruce", "aspen", "fir"))
  crs(vtm2) <- crs(ml)
  ml <- mapAdd(tsf2, ml, filename2 = "tsf2.tif", layerName = "tsf2",
               tsf = "tsf2.tif",
               analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE)
  ml <- mapAdd(vtm2, ml, filename2 = "vtm2.grd", layerName = "vtm2",
               vtm = "vtm2.grd",
               analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE)

  # post hoc analysis of data
  #  use or create a specialized function that can handle the analysesData slot
  ml <- mapAddPostHocAnalysis(map = ml, functionName = "rbindlistAG",
                              postHocAnalysisGroups = "analysisGroup2",
                              postHocAnalyses = "all")


})
