#' @importFrom dplyr tibble mutate
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom glue glue


#' @title Get available bundles for a version
#'
#' @description \strong{Deprecated:} This function is deprecated. Use \code{\link{list_bundles}} instead.
#'
#' @param version Requested version, if not defined, the latest will be used
#' @return dataframe that contains a "bundle_name", "version" and a "domain" column for each available bundle
#' @export
#' @examples
#' # Deprecated - use list_bundles() instead
#' # available_bundles()
#' # available_bundles("0.1")
available_bundles <- function(version = "latest") {
  warning("available_bundles() is deprecated. Use list_bundles() instead.", call. = FALSE)
  raw_dir <- get_raw_dir(version = version)
  directories <- dir(raw_dir, full.names = TRUE)
  bundle_name_paths <- file.path(directories, "bundle_names.csv")


  purrr::map_dfr(bundle_name_paths, parse_bundle_names) |>
    mutate(version = version)
}


#' @title Get concepts for a a single bundle row
#'
#' @description \strong{Deprecated:} This function is deprecated. Use \code{\link{get_bundle_concepts}} instead.
#'
#' @param domain The domain of the bundle.
#' @param id The ID of the bundle.
#' @param version The version of the bundle. Default is "latest".
#' @return A data frame with the concept data.
#' @export
#' @examples
#' # Deprecated - use get_bundle_concepts() instead
#' # concept_by_bundle(domain = "observation", id = "smoking")
concept_by_bundle <- function(domain, id, version = "latest") {
  warning("concept_by_bundle() is deprecated. Use get_bundle_concepts() instead.", call. = FALSE)
  get_raw_dir(version = version, domain, "bundles", glue::glue("{id}.csv")) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::mutate(domain = domain)
}
