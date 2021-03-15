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

    ## Error in bettermc::mclapply(1:2, f) :
    ##   error(s) occured during mclapply; first original message:

    ## Error: non-numeric argument to binary operator

    ## Traceback:
    ## 3: g(as.character(x))
    ## 2: (function (x) 
    ##    g(as.character(x)))(1L)
    ## 1: bettermc::mclapply(1:2, f)

    ## Crash dump avilable. Use 'debugger(attr(*, "dump.frames"))' for debugging.

``` r
# in a non-interactive session a file "last.dump.rds" is created
last.dump <- readRDS("last.dump.rds")
debugger(attr(last.dump[[1L]], "dump.frames"))
```

    ## Message:  non-numeric argument to binary operatorAvailable environments had calls:
    ## 1: (function (x) 
    ## g(as.character(x)))(1)
    ## 2: #1: g(as.character(x))

    ## Enter an environment number, or 0 to exit  
    ## Selection: 0

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

    ## Warning in (function (i) : 2: b

    ##     1: a

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
    ##  bettermc1  413.2336  437.9781  454.7439  446.8591  455.6911  513.3933    10
    ##  bettermc2  800.6699  838.7094  898.8214  885.7591  943.9600 1048.1801    10
    ##  bettermc3 1646.6116 1700.5796 1846.0228 1721.1778 2136.8495 2207.5088    10
    ##  bettermc4 1468.5487 1604.1130 1906.6661 1864.3084 2172.9592 2525.8540    10
    ##   parallel 1487.4475 1630.4703 1951.4752 1955.1688 2050.2836 2720.9375    10

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
    ##  bettermc1  7.990312  7.990312  7.990312  7.990312  7.990312  7.990312     1
    ##   parallel 42.785266 42.785266 42.785266 42.785266 42.785266 42.785266     1

By default, `bettermc` replaces character vectors with objects of type
`char_map` before returning them to the parent process:

``` r
X_comp <- bettermc::compress_chars(X)
str(X_comp)
```

    ## List of 3
    ##  $ chars     : chr [1:999896] "0.221996400272474" "0.219319898169488" "0.627548369579017" "0.1049316492863" ...
    ##  $ idx       : int [1:30000000] 76539 76540 76541 76542 76543 76544 76545 76546 76547 76548 ...
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
    ##  char_map 1.598385 1.624316 1.642065 1.650246 1.663906 1.677565     3
    ##     match 7.093903 7.112660 7.295698 7.131416 7.396595 7.661773     3
