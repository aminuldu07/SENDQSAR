# A helper function to check dependencies
check_dependencies <- function() {
  # Check if devtools is installed
  if (!requireNamespace("devtools", quietly = TRUE)) {
    message("The 'devtools' package is required but not installed. Please install it using:
             install.packages('devtools')")
  }

  # Check if reprtree is installed and install if missing
  if (!requireNamespace("reprtree", quietly = TRUE)) {
    message("The 'reprtree' package is required but not installed. Installing it now from GitHub...")
    devtools::install_github("araastat/reprtree")
  }
}
