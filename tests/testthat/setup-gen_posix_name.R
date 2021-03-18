gen_posix_name <- function(prefix = "bmc_test") {
  timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
  sprintf("/%s_%s", prefix, timestamp)
}
