# bettermc

[![R build
status](https://github.com/gfkse/bettermc/workflows/R-CMD-check/badge.svg)](https://github.com/gfkse/bettermc/actions?workflow=R-CMD-check)
[![codecov](https://codecov.io/gh/gfkse/bettermc/branch/master/graph/badge.svg?token=FYYM156COF)](https://codecov.io/gh/gfkse/bettermc)
[![rchk](https://github.com/gfkse/bettermc/workflows/rchk/badge.svg)](https://github.com/gfkse/bettermc/actions?workflow=rchk)

The `bettermc` package provides a wrapper around the
`parallel::mclapply` function for better performance, error handling and
UX.

## Installation of the Development Version

``` r
# install.packages("devtools")
devtools::install_github("gfkse/bettermc")
```

## Supported Platforms

`bettermc` was originally developed for 64-bit Linux. By now it should
also compile and run on 32-bit systems, and on macOS and Solaris.
However, as stated in the respective help pages, not all features are
supported on macOS. Porting to other POSIX-compliant Unix flavors should
be fairly straightforward. Windows is not supported.

## Features

Here is a short overview on its main features …

### Traceback and Crashdumps

By default, crashdumps and full tracebacks are generated on errors in
child processes:

``` r
g <- function(x) x + 1
f <- function(x) g(as.character(x))
bettermc::mclapply(1:2, f)
```

    ## Error in core(tries_left): error(s) occured during mclapply; first original message:
    ## 
    ## Error: non-numeric argument to binary operator
    ## 
    ## Traceback:
    ## 51: g(as.character(x)) at <text>#2
    ## 50: FUN(X, ...)
    ## 49: withCallingHandlers(list(FUN(X, ...)), warning = whandler, message = mhandler, 
    ##         condition = chandler) at etry.R#34
    ## 48: withCallingHandlers(expr, error = function(e) {
    ...

``` r
# in a non-interactive session a file "last.dump.rds" is created
last.dump <- readRDS("last.dump.rds")

# in an interactive session use debugger() instead of print() for actual debugging
print(attr(last.dump[[1L]], "dump.frames"))
```

    ## $`etry(withCallingHandlers(list(FUN(X, ...)), warning = whandler, message = m`
    ## <environment: 0x55e063f1b038>
    ## 
    ## $`etry.R#34: tryCatch(withCallingHandlers(expr, error = function(e) {\n    if `
    ## <environment: 0x55e063f1afc8>
    ## 
    ## $`tryCatchList(expr, classes, parentenv, handlers)`
    ## <environment: 0x55e063f3a698>
    ## 
    ## $`tryCatchOne(expr, names, parentenv, handlers[[1]])`
    ## <environment: 0x55e063f3dba8>
    ## 
    ## $`doTryCatch(return(expr), name, parentenv, handler)`
    ## <environment: 0x55e063f4c430>
    ## 
    ## $`etry.R#34: withCallingHandlers(expr, error = function(e) {\n    if ("max.lin`
    ## <environment: 0x55e063f52a18>
    ## 
    ## $`etry.R#34: withCallingHandlers(list(FUN(X, ...)), warning = whandler, messa`
    ## <environment: 0x55e063f68868>
    ## 
    ## $`FUN(X, ...)`
    ## <environment: 0x55e064148cc8>
    ## 
    ## $`<text>#2: g(as.character(x))`
    ## <environment: 0x55e064148bb0>
    ## 
    ## attr(,"error.message")
    ## [1] "non-numeric argument to binary operator\n\n"
    ## attr(,"class")
    ## [1] "dump.frames"

### Output, Messages and Warnings

In contrast to `parallel::mclapply`, neither output nor messages nor
warnings from the child processes are lost. All of these can be
forwarded to the parent process and are prefix with the index of the
function invocation from which they originate:

``` r
f <- function(i) {
  if (i == 1) message(letters[i])
  else if (i == 2) warning(letters[i])
  else print(letters[i])
  
  i
}
ret <- bettermc::mclapply(1:4, f)
```

    ##     3: [1] "c"
    ##     4: [1] "d"

    ## Warning in FUN(X, ...): 2: b

    ##     1: a

Similarly, other conditions can also be re-signaled in the parent
process.

### POSIX Shared Memory

Many types of objects can be returned from the child processes using
POSIX shared memory. This includes e.g. logical, numeric, complex and
raw vectors and arrays as well as factors. In doing so, the overhead of
getting larger results back into the parent R process is reduced:

``` r
X <- data.frame(
  x = runif(3e7),
  y = sample(c(TRUE, FALSE), 3e7, TRUE),
  z = 1:3e7
)
f <- function(i) X

microbenchmark::microbenchmark(
  bettermc1 = bettermc::mclapply(1:2, f, mc.share.copy = FALSE),
  bettermc2 = bettermc::mclapply(1:2, f),
  bettermc3 = bettermc::mclapply(1:2, f, mc.share.vectors = FALSE),
  bettermc4 = bettermc::mclapply(1:2, f, mc.share.vectors = FALSE, mc.shm.ipc = FALSE),
  parallel = parallel::mclapply(1:2, f),
  times = 10, setup = gc()
)
```

    ## Unit: milliseconds
    ##       expr       min        lq      mean    median        uq       max neval
    ##  bettermc1  320.0410  350.3665  369.2490  370.6599  384.5358  414.8268    10
    ##  bettermc2  603.9296  649.0813  676.4485  682.4288  708.9244  742.5663    10
    ##  bettermc3 1116.2009 1153.9187 1277.9979 1190.5501 1270.3877 1802.2334    10
    ##  bettermc4 1200.3030 1313.3324 1591.2849 1544.1784 1852.0342 2004.0813    10
    ##   parallel 1113.0253 1416.4247 1565.6660 1528.5397 1748.7820 2040.8899    10

In examples `bettermc1` and `bettermc2`, the child processes place the
columns of the return value `X` in shared memory. The object which needs
to be serialized for transfer from child to parent processes hence
becomes:

``` r
X_shm <- bettermc:::vectors2shm(X, name_prefix = "/bettermc_README_")
str(X_shm)
```

    ## 'data.frame':    30000000 obs. of  3 variables:
    ##  $ x:List of 6
    ##   ..$ name      : chr "/bettermc_README_1"
    ##   ..$ type      : int 14
    ##   ..$ length    : num 3e+07
    ##   ..$ size      : num 2.4e+08
    ##   ..$ attributes: NULL
    ##   ..$ copy      : logi TRUE
    ##   ..- attr(*, "class")= chr "shm_obj"
    ##  $ y:List of 6
    ##   ..$ name      : chr "/bettermc_README_2"
    ##   ..$ type      : int 10
    ##   ..$ length    : num 3e+07
    ##   ..$ size      : num 1.2e+08
    ##   ..$ attributes: NULL
    ##   ..$ copy      : logi TRUE
    ##   ..- attr(*, "class")= chr "shm_obj"
    ##  $ z: int  1 2 3 4 5 6 7 8 9 10 ...

Column `z` is an `ALTREP` and, because it can be serialized efficiently,
is left alone by default. The parent process can recover the original
object from `X_shm`:

``` r
Y <- bettermc:::shm2vectors(X_shm)
stopifnot(identical(X, Y))
```

The internal functions `vectors2shm()` and `shm2vectors()` recursively
walk the return value and apply the exported functions `copy2shm()` and
`allocate_from_shm()`, respectively.

In `bettermc1`, the shared memory objects are used directly by the
parent process. In `bettermc2`, which is the default, new vectors are
allocated in the parent process and the data is merely copied from the
shared memory objects, which are freed afterwards. See `?copy2shm` for
more details on this topic and why the slower `mc.share.copy = TRUE`
might be a sensible default.

In `bettermc3`, the original `X` is serialized and the resulting raw
vector is placed in shared memory, from where it is deserialized in the
parent process.

`bettermc4` does not involve any POSIX shared memory and hence is
equivalent to `parallel`, i.e. the original `X` is serialized and
transferred to the parent process using pipes.

### Character Compression

In practice, character vectors often contain a substantial amount of
duplicated values. This is exploited by `bettermc` to speed up the
returning of larger character vectors from child processes:

``` r
X <- rep(as.character(runif(1e6)), 30)
f <- function(i) X
microbenchmark::microbenchmark(
  bettermc1 = bettermc::mclapply(1:2, f),
  parallel =  parallel::mclapply(1:2, f),
  times = 1, setup = gc()
)
```

    ## Unit: seconds
    ##       expr       min        lq      mean    median        uq       max neval
    ##  bettermc1  5.378616  5.378616  5.378616  5.378616  5.378616  5.378616     1
    ##   parallel 32.200891 32.200891 32.200891 32.200891 32.200891 32.200891     1

By default, `bettermc` replaces character vectors with objects of type
`char_map` before returning them to the parent process:

``` r
X_comp <- bettermc::compress_chars(X)
str(X_comp)
```

    ## List of 3
    ##  $ chars     : chr [1:999869] "0.427193737355992" "0.165682514430955" "0.307766162557527" "0.5674047768116" ...
    ##  $ idx       : int [1:30000000] 15549 15550 15551 15552 15553 15554 15555 15556 15557 15558 ...
    ##  $ attributes: NULL
    ##  - attr(*, "class")= chr "char_map"

The important detail here is the length of the `chars` vector, which
just contains the unique elements of `X` and hence is significantly
faster to (de)serialize than the original vector. The parent process can
recover the original character vectors:

``` r
Y <- bettermc::uncompress_chars(X_comp)
stopifnot(identical(X, Y))
```

The functions `compress_chars()` and `uncompress_chars()` recursively
walk the return value and apply the functions `char_map()` and
`map2char()`, respectively.

`char_map()` is implemented using a radix sort, which makes it very
efficient:

``` r
microbenchmark::microbenchmark(
  char_map = bettermc::char_map(X),
  match = {chars <- unique(X); idx <- match(X, chars)},
  times = 3, setup = gc()
)
```

    ## Unit: seconds
    ##      expr      min       lq     mean   median       uq      max neval
    ##  char_map 1.767851 1.815203 1.836038 1.862556 1.870132 1.877708     3
    ##     match 4.400479 4.450915 4.555137 4.501351 4.632466 4.763580     3

### Retries

`bettermc` supports automatic retries on both fatal and non-fatal
errors. `mc.force.fork` ensures that `FUN` is called in a child process,
even if `X` is of length 1. This is useful if `FUN` might encounter a
fatal error and we want to protect the parent process against it. With
retires, `length(X)` might drop to 1 if all other values could already
be processed. This is also why we need `mc.force.fork` in the following
example:

``` r
set.seed(123)
res <-
  bettermc::mclapply(1:20, function(i) {
    r <- runif(1)
    if (r < 0.25)
      system(paste0("kill ", Sys.getpid()))
    else if (r < 0.5)
      stop(i)
    else
      i
  }, mc.retry = 50, mc.cores = 10, mc.force.fork = TRUE)
```

    ## at least one scheduled core did not return results; maybe it was killed (by the Linux Out of Memory Killer ?) or there was a fatal error in the forked process(es)

    ## error(s) occured during mclapply; first original message:
    ## 
    ## Error: 2
    ## 
    ## Traceback:
    ## 46: stop(i) at <text>#5
    ## 45: FUN(X, ...)
    ## 44: withCallingHandlers(list(FUN(X, ...)), warning = whandler, message = mhandler, 
    ##         condition = chandler) at etry.R#34
    ## 43: withCallingHandlers(expr, error = function(e) {
    ...

    ## at least one scheduled core did not return results; maybe it was killed (by the Linux Out of Memory Killer ?) or there was a fatal error in the forked process(es)

    ## error(s) occured during mclapply; first original message:
    ## 
    ## Error: 2
    ## 
    ## Traceback:
    ## 46: stop(i) at <text>#5
    ## 45: FUN(X, ...)
    ## 44: withCallingHandlers(list(FUN(X, ...)), warning = whandler, message = mhandler, 
    ##         condition = chandler) at etry.R#34
    ## 43: withCallingHandlers(expr, error = function(e) {
    ...

    ## error(s) occured during mclapply; first original message:
    ## 
    ## Error: 4
    ## 
    ## Traceback:
    ## 46: stop(i) at <text>#5
    ## 45: FUN(X, ...)
    ## 44: withCallingHandlers(list(FUN(X, ...)), warning = whandler, message = mhandler, 
    ##         condition = chandler) at etry.R#34
    ## 43: withCallingHandlers(expr, error = function(e) {
    ...

    ## at least one scheduled core did not return results; maybe it was killed (by the Linux Out of Memory Killer ?) or there was a fatal error in the forked process(es)

``` r
stopifnot(identical(res, as.list(1:20)))
```

Additionally, it is possible to automatically decrease the number of
cores with every retry by specifying a negative value for `mc.retry`.
This is useful if we expect failures to be caused simply by too many
concurrent processes, e.g. if system load or the size if input data is
unpredictable and might lead to the Linux Out Of Memory Killer stepping
in. In such a case it makes sense to retry using fewer concurrent
processes:

``` r
ppid <- Sys.getpid()
res <-
  bettermc::mclapply(1:20, function(i) {
    Sys.sleep(0.25)  # wait for the other child processes
    number_of_child_processes <- length(system(paste0("pgrep -P ", ppid), intern = TRUE))
    if (number_of_child_processes >= 5) system(paste0("kill ", Sys.getpid()))
    i
  }, mc.retry = -3, mc.cores = 10, mc.force.fork = TRUE)
```

    ## at least one scheduled core did not return results; maybe it was killed (by the Linux Out of Memory Killer ?) or there was a fatal error in the forked process(es)
    ## at least one scheduled core did not return results; maybe it was killed (by the Linux Out of Memory Killer ?) or there was a fatal error in the forked process(es)

``` r
stopifnot(identical(res, as.list(1:20)))
```
