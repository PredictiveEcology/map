# map

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/map)](https://cran.r-project.org/package=map)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/map)](https://cran.r-project.org/package=map)
[![R build status](https://github.com/PredictiveEcology/map/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/map/actions)
<!-- badges: end -->

Defines a meta class of geographical objects, the `map` class, which is a collection of map objects (`sp`, `raster`, `sf`), with a number of metadata additions to enable powerful methods, *e.g.*, for `leaflet`, reproducible GIS etc.

## Installation

### Current release

[![R build status](https://github.com/PredictiveEcology/map/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/PredictiveEcology/map/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/map/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/map?branch=master)

**Install from CRAN:**

```r
#install.packages("map") ## not yet on CRAN
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/map", dependencies = TRUE) 
```

### Development version

[![R build status](https://github.com/PredictiveEcology/map/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/map/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/map/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/map?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/map", ref = "development", dependencies = TRUE) 
```
