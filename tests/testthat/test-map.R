
test_that("prepInputs doesn't work", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ml <- mapAdd(layerName = "LandWeb Study Area", targetFile = "landweb_ltfc_v6.shp",
               alsoExtract = c("landweb_ltfc_v6.dbf", "landweb_ltfc_v6.prj",
                               "landweb_ltfc_v6.sbn", "landweb_ltfc_v6.sbx", "landweb_ltfc_v6.shx"),
               targetCRS = CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"),
               sourceURL = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6",
               columnNameForLabels = "Name", isStudyArea = TRUE
  )

  # Make a random small study area
  seed <- 863
  set.seed(seed)
  sp2 <- SpaDES.tools::randomPolygon(studyArea(ml), 4e5)
  ml <- mapAdd(object = sp2, map = ml, filename2 = FALSE,
               layerName = "Small Study Area",
               columnNameForLabels = "Name", isStudyArea = TRUE,
               filename1 = NULL, overwrite = TRUE
  )

  ml <- mapAdd(sourceURL = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
               map = ml, leaflet = TRUE, studyArea = studyArea(ml, 2),
               alsoExtract = c("Age1.tfw", "Age1.tif.aux.xml", "Age1.tif.ovr",
                               "Age1.tif.vat.cpg", "Age1.tif.vat.dbf", "Age1.tif.vat.dbf.xml"),
               targetFile = "Age1.tif", overwrite = TRUE, filename2 = "Age.tif",
               layerName = "Age") # dots include things like method = "ngb" for projectRaster
})
