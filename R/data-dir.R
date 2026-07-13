#' @title Get data-raw directory path
#'
#' @description Returns the path to the data-raw directory. This function can
#'   be mocked in tests to use a temporary directory.
#'
#' @return Character string with path to data-raw directory
#' @noRd
get_data_raw_dir <- function() {
  data_dir <- system.file("data-raw", package = "omopbundles")

  if (!dir.exists(data_dir)) {
    stop("data-raw directory not found. Package may need to be reinstalled.")
  }
  data_dir
}
