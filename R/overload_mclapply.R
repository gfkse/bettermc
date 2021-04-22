#' Overload \code{\link[parallel:mclapply]{parallel::mclapply}} With
#' \code{\link[bettermc:mclapply]{bettermc::mclapply}}
#'
#' Enable the use of \code{\link[bettermc:mclapply]{bettermc::mclapply}} by
#' third-party packages originally using \code{mclapply} from the parallel
#' package, e.g. doMC or rstan. This is achieved by replacing the
#' \code{mclapply}-function in various environments.
#' \code{undo_overload_mclapply} re-installs the original
#' \code{\link[parallel:mclapply]{parallel::mclapply}}.
#'
#' @param parallel_namespace should \code{mclapply} be overloaded in the
#'   namespace of the parallel package (namespace:parallel)? Use this if the
#'   third-party package calls \code{mclapply} using the double colon operator,
#'   e.g. as in \code{rstan::sampling}.
#' @param parallel_package should \code{mclapply} be overloaded on the search
#'   path iff the parallel package is attached (package:parallel)? This should
#'   generally not be required.
#' @param imports the name of packages for which \code{mclapply} should be
#'   overloaded in their imports, e.g. "doMC" for doMC:imports. If the package
#'   is not already loaded, it will be loaded first. The special value "all"
#'   means all loaded packages. Use this if the third-party package imports
#'   \code{mclapply} in its NAMESPACE file, e.g. as the doMC package does.
#' @param defaults a list named by one or more of the formal arguments of
#'   \code{\link[bettermc:mclapply]{bettermc::mclapply}} providing new defaults
#'   for the overloaded function.
#' @param fixed_args a list named by one or more of the formal arguments of
#'   \code{\link[bettermc:mclapply]{bettermc::mclapply}}, which will fix these
#'   to the provided values in the overloaded function.
#'
#' @examples
#' if (tolower(Sys.info()[["sysname"]]) != "windows") {
#'   doMC::registerDoMC(2L)
#'
#'   # fix mc.set.seed arg to NA in order to avoid modifications by doMC:::doMC
#'   overload_mclapply(imports = "doMC", fixed_args = list(mc.set.seed = NA))
#'
#'   set.seed(123)
#'   ret1 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
#'   set.seed(123)
#'   ret2 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
#'   stopifnot(identical(ret1, ret2))
#'
#'   undo_overload_mclapply(imports = "doMC")
#'
#'   # back to using parallel::mclapply under the hood -> seeding has no effect
#'   set.seed(123)
#'   ret1 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
#'   set.seed(123)
#'   ret2 <- foreach::`%dopar%`(foreach::foreach(i = 1:4), runif(1))
#'   stopifnot(!identical(ret1, ret2))
#' }
#'
#' @section Windows Support: Fully supported on Windows.
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options:
#'    alt='[Experimental]'}}}{\strong{[Experimental]}}
#'
#'   It is unlikely that these functions will be accepted on CRAN.
#'
#' @return These functions return NULL invisibly and are called for their side
#'   effects.
#'
#' @export
overload_mclapply <- function(parallel_namespace = FALSE,
                              parallel_package = FALSE,
                              imports = character(),
                              defaults = list(),
                              fixed_args = list()) {

  checkmate::assert_flag(parallel_namespace)
  checkmate::assert_flag(parallel_package)
  checkmate::assert_character(imports, any.missing = FALSE, min.chars = 1L)
  checkmate::assert_list(defaults)
  if (length(defaults)) {
    checkmate::assert_names(names(defaults), type = "unique",
                            subset.of = names(formals(mclapply)),
                            disjunct.from = "...")
  }
  checkmate::assert_list(fixed_args)
  if (length(fixed_args)) {
    checkmate::assert_names(names(fixed_args), type = "unique",
                            subset.of = names(formals(mclapply)),
                            disjunct.from = "...")
  }

  mclapply <- mclapply

  if (length(defaults)) {
    frmls <- formals(mclapply)
    frmls[names(defaults)] <- defaults
    formals(mclapply) <- frmls
  }

  if (length(fixed_args)) {
    call_args <- formals(mclapply)
    call_args <- sapply(names(call_args), as.name, simplify = FALSE)
    call_args[names(fixed_args)] <- fixed_args

    body(mclapply) <- as.call(c(as.name("mclapply"), call_args))
  }

  if (parallel_namespace) {
    replace_binding(asNamespace("parallel"), "mclapply", mclapply)
  }

  if (parallel_package) {
    env <- try(as.environment("package:parallel"), silent = TRUE)
    if (!inherits(env, "try-error")) {
      replace_binding(env, "mclapply", mclapply)
    }
  }

  if (length(imports)) {
    if (identical(imports, "all")) {
      imports <- loadedNamespaces()
    }
    imports <- setdiff(imports, "base")

    pns <- asNamespace("parallel")
    for (import in imports) {
      env <- parent.env(asNamespace(import))
      f <- get0("mclapply", env, mode = "function", inherits = FALSE)
      if (!is.null(f) && identical(environment(f), pns)) {
        replace_binding(env, "mclapply", mclapply)
      }
    }
  }

  invisible()
}

#' @rdname overload_mclapply
#' @export
undo_overload_mclapply <- function(parallel_namespace = FALSE,
                                   parallel_package = FALSE,
                                   imports = character()) {

  checkmate::assert_flag(parallel_namespace)
  checkmate::assert_flag(parallel_package)
  checkmate::assert_character(imports, any.missing = FALSE, min.chars = 1L)

  if (parallel_namespace) {
    replace_binding(asNamespace("parallel"), "mclapply", parallel_mclapply)
  }

  if (parallel_package) {
    env <- try(as.environment("package:parallel"), silent = TRUE)
    if (!inherits(env, "try-error")) {
      replace_binding(env, "mclapply", parallel_mclapply)
    }
  }

  if (length(imports)) {
    if (identical(imports, "all")) {
      imports <- loadedNamespaces()
    }
    imports <- setdiff(imports, "base")

    bns <- asNamespace("bettermc")
    for (import in imports) {
      env <- parent.env(asNamespace(import))
      f <- get0("mclapply", env, mode = "function", inherits = FALSE)
      if (!is.null(f) && identical(environment(f), bns)) {
        replace_binding(env, "mclapply", parallel_mclapply)
      }
    }
  }

  invisible()
}

# utility function to replace a symbol in a locked environment
# (based on https://github.com/r-lib/covr/blob/master/R/parallel.R)
replace_binding <- function(env, name, value) {
  unlock <- get('unlockBinding') # to fool r CMD check
  lock <-  get('lockBinding')
  unlock(name, env)
  assign(name, value, env)
  lock(name, env)
}
