OSTYPE <-
  if (startsWith(tolower(Sys.info()["sysname"]), "linux")) {
    "linux"
  } else if (startsWith(tolower(Sys.info()["sysname"]), "darwin")) {
    "macos"
  } else if (startsWith(tolower(Sys.info()["sysname"]), "sunos")) {
    "solaris"
  } else {
    stop("This does not look like a Linux, Solaris or macOS. Aborting.")
  }
