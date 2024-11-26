# map (development version)

* drop support for R 4.0 due to changes in dependency packages;
* remove retiring dependency packages `sp`, `rgeos`, `rgdal` in favour of `terra` + `sf`;
* remove dependency package `gdalUtils` in favour of `sf` + `terra`;
* use most recent `tiler` version (0.3.0);
* use `digest::digest(x, algo = "spooky")` instead of `fastdigest` (which was archived on CRAN);

# map v0.0.3

* maximum number of threads for parallel operations limited by package option `map.maxNumCores`, which defaults to `min(getOption("Ncpus"), parallel::detectCores())`.

# map v0.0.1

* initial development version
