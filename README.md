# bettermc

The `bettermc` package provides a wrapper around the
`parallel::mclapply` function for better performance, error handling and
UX.

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
    ##  bettermc1  290.0381  301.7096  320.2449  312.3831  340.7723  375.8056    10
    ##  bettermc2  574.2776  583.0323  652.8619  600.7823  667.3698  970.8116    10
    ##  bettermc3 1017.0415 1054.2873 1092.2762 1076.4731 1106.3628 1300.8237    10
    ##  bettermc4  873.2696 1044.7785 1171.3308 1157.7447 1289.2521 1446.0259    10
    ##   parallel 1067.6360 1195.5375 1371.1622 1257.5502 1353.7738 2050.8321    10

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
    ##  bettermc1  4.547525  4.547525  4.547525  4.547525  4.547525  4.547525     1
    ##   parallel 31.059487 31.059487 31.059487 31.059487 31.059487 31.059487     1

By default, `bettermc` replaces character vectors with objects of type
`char_map` before returning them to the parent process:

``` r
X_comp <- bettermc:::compress_chars(X)
str(X_comp)
```

    ## List of 3
    ##  $ chars     : chr [1:999856] "0.956617942545563" "0.777563852956519" "0.742107977392152" "0.901834991294891" ...
    ##  $ idx       : int [1:30000000] 28012 28013 28014 28015 28016 28017 28018 28019 28020 28021 ...
    ##  $ attributes: NULL
    ##  - attr(*, "class")= chr "char_map"

The important detail here is the length of the `chars` vector, which
just contains the unique elements of `X` and hence is significantly
faster to (de)serialize than the original vector. The parent process can
recover the original character vectors:

``` r
Y <- bettermc:::uncompress_chars(X_comp)
stopifnot(identical(X, Y))
```

The internal functions `compress_chars()` and `uncompress_chars()`
recursively walk the return value and apply the exported functions
`char_map()` and `map2char()`, respectively.

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
    ##  char_map 1.288056 1.303647 1.312008 1.319238 1.323984 1.328730     3
    ##     match 3.942771 3.980546 3.996469 4.018322 4.023318 4.028314     3
