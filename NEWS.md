# map 1.1.2

- `makeTiles()` now checks for python and GDAL support, i.e., for Windows, which requires 'OSGeos4W' (#19);
- new option `map.tileRetry` with default value `3L`, specifying the number of attempts to make tiles (#19);

# map 1.1.1

* drop support for R 4.1 and 4.2 due to changes in dependency packages;
* move `withr` from Suggests to Imports, as it's used in examples;
* improved documentation

# map v1.1.0

* drop support for R 4.0 due to changes in dependency packages;
* remove retiring dependency packages `sp`, `rgeos`, `rgdal` in favour of `terra` + `sf`;
* remove dependency package `gdalUtils` in favour of `sf` + `terra`;
* use most recent `tiler` version (0.3.0);
* use `digest::digest(x, algo = "spooky")` instead of `fastdigest` (which was archived on CRAN);
* remove unused dependencies `knitr` and `rmarkdown`;

# map v0.0.3

* maximum number of threads for parallel operations limited by package option `map.maxNumCores`, which defaults to `min(getOption("Ncpus"), parallel::detectCores())`.

# map v0.0.1

* initial development version
