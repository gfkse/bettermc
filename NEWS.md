# bettermc [v1.0.1.9000]

## Bug Fixes
* fix wrong length of affinity.list if `mc.force.fork == TRUE && length(X) == 1`
* fix tiny memory leaks reported by Valgrind in `allocate_from_shm()`
* fix invalid write reported by Valgrind in `allocate_from_shm()`
* fix test failure on macOS with a page size of more than 4 KiB

# bettermc [v1.0.1]
* initial CRAN release
