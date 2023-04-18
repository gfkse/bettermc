`%\\%` <- function(x, y) {
  paste(x, y)
}

`%+%` <- function(x, y) {
  paste0(x, y)
}

is.uneval.promise <- function(name, env) {
  .Call(C_is_uneval_promise, name = name, env = env)
}

is.eval.promise2missing.arg <- function(name, env) {
  .Call(C_is_eval_promise_to_missing_arg, name = name, env = env)
}

is.missing.arg <- function(name, env) {
  penv <- parent.env(env)
  on.exit(parent.env(env) <- penv)
  parent.env(env) <- baseenv()
  eval(parse(text = paste0("base::missing(`", name, "`)")), env)
}

make_root_stop <- function(call = sys.call(-1L)) {
  force(call)
  function(...) stop(simpleError(paste0(..., collapse = ""), call = call))
}

make_root_warning <- function(call = sys.call(-1L)) {
  force(call)
  function(...) warning(simpleWarning(paste0(..., collapse = ""), call = call))
}

set_timeout <- function(timeout, type = c("elapsed", "cpu"), signal = 2L) {
  type <- match.arg(type)
  .Call(C_set_timeout, seconds = as.integer(timeout),
        clock_type = match(type, c("elapsed", "cpu")),
        signal = as.integer(signal))
}

disable_timeout <- function(timerid) {
  invisible(.Call(C_disable_timeout, timerid = timerid))
}

#' CPU Pools
#'
#' Create and destroy CPU pools to be passed to \code{\link{mclapply}} as
#' argument \code{mc.cpu.pool}.
#'
#' @param ncpu the number of CPUs in the pool.
#'
#' @export
create_cpu_pool <- function(ncpu = parallel::detectCores()) {
  checkmate::assert_number(ncpu, lower = 1, finite = TRUE)
  p <- semv_open(ncpu)
  class(p) <- c("bettermc_cpu_pool", class(p))
  p
}

#' @rdname create_cpu_pool
#'
#' @param p an object as returned by \code{create_cpu_pool}.
#'
#' @export
destroy_cpu_pool <- function(p) {
  checkmate::assert_class(p, c("bettermc_cpu_pool", "semv"), ordered = TRUE)
  semv_unlink(p)
}
