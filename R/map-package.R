#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import methods
#' @importFrom data.table copy data.table rbindlist set setDT
#' @importFrom digest digest
#' @importFrom parallel stopCluster
#' @importFrom parallelly availableCores
#' @importFrom pemisc getLocalArgsFor identifyVectorArgs makeOptimalCluster Map2 MapOrDoCall
#' @importFrom quickPlot whereInStack
#' @importFrom raster area compareCRS crs extent filename getValues metadata ncell
#' @importFrom raster projectRaster raster rasterOptions setValues writeRaster
#' @importFrom reproducible .requireNamespace .robustDigest
#' @importFrom reproducible asPath assessDataType Cache compareNA cropInputs Filenames fixErrors
#' @importFrom reproducible postProcess prepInputs preProcess projectInputs projectTo writeOutputs
#' @importFrom sf as_Spatial gdal_polygonize st_area st_as_sf st_bbox st_crs st_read st_as_sf st_zm
#' @importFrom stats na.omit
#' @importFrom terra as.polygons ext disagg is.factor levels rast same.crs setValues
#' @importFrom terra values vect writeRaster writeVector
#' @importFrom tiler tile tiler_options
#' @importFrom tools file_path_sans_ext
#' @importFrom utils capture.output getS3method
#' @importFrom withr deferred_run local_dir local_options local_tempdir
## usethis namespace: end
NULL
