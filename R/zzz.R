# This function runs when the package is attached
.onAttach <- function(libname, pkgname) {
  check_dependencies()
  packageStartupMessage("Welcome to SENDQSAR! All necessary dependencies have been checked.")
}
