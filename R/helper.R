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
