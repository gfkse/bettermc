OSTYPE <-
  if (startsWith(tolower(Sys.info()["sysname"]), "linux")) {
    "linux"
  } else if (startsWith(tolower(Sys.info()["sysname"]), "darwin")) {
    "macos"
  } else if (startsWith(tolower(Sys.info()["sysname"]), "sunos")) {
    "solaris"
  } else if (startsWith(tolower(Sys.info()["sysname"]), "windows")) {
    "windows"
  } else {
    stop("This does not look like a Linux, Solaris, macOS or Windows. Aborting.")
  }

.onAttach = function(libname, pkgname) {
  if (OSTYPE == "windows") {
    packageStartupMessage("Many of the features of the bettermc-package are not supported on Windows. ",
                          "Please consult the 'Windows Support'-sections in the help pages for more details.")
  }
}
