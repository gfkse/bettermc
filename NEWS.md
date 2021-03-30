# bettermc [v1.0.1.9000]

## Bug Fixes
* on the first call of `mclapply()` in a session, the environment variable MC_CORES was not respected
* fix wrong length of affinity.list if `mc.force.fork == TRUE && length(X) == 1`
* fix tiny memory leaks reported by Valgrind in `allocate_from_shm()`
* fix invalid write reported by Valgrind in `allocate_from_shm()`
* mark memory returned by custom allocator in `allocate_from_shm()` for Valgrind as defined (cf. https://r.789695.n4.nabble.com/custom-allocators-Valgrind-and-uninitialized-memory-td4768304.html)
* fix test failure on macOS with a page size of more than 4 KiB

# bettermc [v1.0.1]
* initial CRAN release
