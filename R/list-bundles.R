#' @importFrom readr read_csv
#' @importFrom dplyr arrange

#' @title List all available bundles
#'
#' @description Returns a data frame with metadata for all available bundles.
#'
#' @return Data frame with columns: bundle_id, bundle_name, description,
#'   created_date, modified_date
#' @export
#'
#' @examples
#' # List all bundles
#' list_bundles()
list_bundles <- function() {
  bundles <- read_raw_data("bundles")

  # Sort by bundle_id for consistent output
  dplyr::arrange(bundles, .data$bundle_id)
}
