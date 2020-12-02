`%\\%` <- function(x, y) {
  paste(x, y)
}

`%+%` <- function(x, y) {
  paste0(x, y)
}

is.uneval.promise <- function(name, env) {
  .Call(C_is_uneval_promise, name = name, env = env)
}
