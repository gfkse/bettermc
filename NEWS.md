# bettermc [v1.1.1]

## New Features

### Windows Support
This package can now also be installed on Windows, where it offers a reduced set of features.
Most notably, `bettermc::mclapply()` (just like `parallel::mclapply()`) falls back on serial execution, since forking is not available on Windows.
Still, this allows code employing `bettermc::mclapply()`, which was originally developed for Linux or macOS, to be run on Windows.

### Seeding
* restore the state of the random number generator (RNG) at the end of `mclapply()` to what it was before; this makes the RNG state of the parent process immune to changes to arguments such as `mc.cores` or `mc.force.fork`
* new argument `mc.retry.fixed.seed` to `mclapply()`: `TRUE` invokes `FUN()` for a particular element of `X` with the same fixed seed on every retry; default is `FALSE`

### Misc
* the environment variable "BMC_RETRY" now indicates the current retry ("0" means first try, a value of "1" first *re*try, etc.)
* new argument `mc.retry.silent` to `mclapply()`: `TRUE` suppresses the messages indicating failures during intermediate retires; default is `FALSE`

## Bug Fixes
* on retires, output, messages etc. might have been prefixed with the wrong index w.r.t. `X`
* on the first call of `mclapply()` in a session, the environment variable MC_CORES was not respected
* fix wrong length of affinity.list if `mc.force.fork == TRUE && length(X) == 1`
* fix tiny memory leaks reported by Valgrind in `allocate_from_shm()`
* fix invalid write reported by Valgrind in `allocate_from_shm()`
* mark memory returned by custom allocator in `allocate_from_shm()` for Valgrind as defined (cf. https://r.789695.n4.nabble.com/custom-allocators-Valgrind-and-uninitialized-memory-td4768304.html)
* fix test failure on macOS with a page size of more than 4 KiB

# bettermc [v1.0.1]
* initial CRAN release
