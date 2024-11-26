# map (development version)

* drop support for R 4.0 due to changes in dependency packages;
* use `digest::digest(x, algo = "spooky")` instead of `fastdigest` (which was archived on CRAN);
* remove unused dependencies `knitr` and `rmarkdown`;

# map v0.0.3

* maximum number of threads for parallel operations limited by package option `map.maxNumCores`, which defaults to `min(getOption("Ncpus"), parallel::detectCores())`.

# map v0.0.1

* initial development version
