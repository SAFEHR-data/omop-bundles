#' @importFrom readr read_csv write_csv

#' @title Read raw data from the data-raw directory
#'
#' @description Reads raw data from the data-raw directory into a data frame.
#'
#' @param table Character. The table to read.
#' @return A data frame.
#' @noRd
read_raw_data <- function(table = c("bundles", "concepts")) {
  table <- match.arg(table)
  data_dir <- get_data_raw_dir()
  filepath <- file.path(data_dir, glue::glue("{table}.csv"))

  if (!file.exists(filepath)) {
    stop(glue::glue("{filepath} not found. Package may need to be reinstalled or data migrated."))
  }

  col_types <- list(
    bundles = "cccDD",
    concepts = "cill"
  )

  readr::read_csv(filepath, show_col_types = FALSE, col_types = col_types[[table]])
}
