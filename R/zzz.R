OSTYPE <-
  if (startsWith(tolower(Sys.info()["sysname"]), "linux")) {
    "linux"
  } else if (startsWith(tolower(Sys.info()["sysname"]), "darwin")) {
    "macos"
  } else {
    stop("This does not look like a Linux or macOS. Aborting.")
  }
