# map

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/map)](https://cran.r-project.org/package=map)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/map)](https://cran.r-project.org/package=map)

Defines a meta class of geographical objects, the `map` class, which is a collection of map objects (`sp`, `raster`, `sf`), with a number of metadata additions to enable powerful methods, *e.g.*, for `leaflet`, reproducible GIS etc.

## Installation

### Current release

[![Build Status](https://travis-ci.org/PredictiveEcology/map.svg?branch=master)](https://travis-ci.org/PredictiveEcology/map)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/map/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/map/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/map?branch=master)

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

[![Build Status](https://travis-ci.org/PredictiveEcology/map.svg?branch=development)](https://travis-ci.org/PredictiveEcology/map)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/development?svg=true)](https://ci.appveyor.com/project/achubaty/map/branch/development)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/map/badge.svg?branch=development)](https://coveralls.io/github/PredictiveEcology/map?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/map", ref = "development", dependencies = TRUE) 
```
