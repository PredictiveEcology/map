# map (development version)

* drop support for R 4.0 due to changes in dependency packages

# map v0.0.3

* maximum number of threads for parallel operations limited by package option `map.maxNumCores`, which defaults to `min(getOption("Ncpus"), parallel::detectCores())`.

# map v0.0.1

* initial development version
