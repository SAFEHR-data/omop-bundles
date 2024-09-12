#' @importFrom dplyr tibble mutate
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom glue glue


#' @title Get available bundles for a version
#'
#' @description If a bundle has multiple names, then the id will be duplicated across rows
#'
#' @param version Requested version, if not defined, the latest will be used
#' @return dataframe that contains a "concept_name", "version" and a "domain" column for each available concept
#' @export
#' @examples
#' available_bundles()
#' available_bundles("0.1")
available_bundles <- function(version = "latest") {
  raw_dir <- omopbundles:::get_raw_dir(version = version)
  directories <- dir(raw_dir, full.names = TRUE)
  bundle_name_paths <- file.path(directories, "bundle_names.csv")


  purrr::map_dfr(bundle_name_paths, omopbundles:::parse_bundle_names) |>
    mutate(version = version)
}


#' @title Get concepts for a a single bundle row
#'
#' @description Retrieves concept data for a specific bundle.
#'
#' @param domain The domain of the bundle.
#' @param id The ID of the bundle.
#' @param version The version of the bundle. Default is "latest".
#' @return A data frame with the concept data.
#' @export
#' @examples
#' # Usage with available_bundles, from a single row
#' smoking_info <- available_bundles() |> dplyr::filter(concept_name == "smoking")
#' concept_by_bundle(domain = smoking_info$domain, id = smoking_info$id, version = smoking_info$version)
#' # Using if you know the details directly
#' concept_by_bundle(domain = "observation", id = "smoking")
concept_by_bundle <- function(domain, id, version = "latest") {
  omopbundles:::get_raw_dir(version = version, domain, "bundles", glue::glue("{id}.csv")) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::mutate(domain = domain)
}
