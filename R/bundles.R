#' @importFrom dplyr tibble mutate
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom glue glue


#' @title Get available bundles for a version
#'
#' @description If a bundle has multiple names, then the id will be duplicated across rows
#'
#' @param version Requested version, if not defined, the latest will be used
#' @return dataframe that contains a "bundle_name", "version" and a "domain" column for each available bundle
#' @export
#' @examples
#' available_bundles()
#' available_bundles("0.1")
available_bundles <- function(version = "latest") {
  raw_dir <- get_raw_dir(version = version)
  directories <- dir(raw_dir, full.names = TRUE)
  bundle_name_paths <- file.path(directories, "bundle_names.csv")


  purrr::map_dfr(bundle_name_paths, parse_bundle_names) |>
    mutate(version = version)
}


#' @title Get concepts for a a single bundle row
#'
#' @description Retrieves concept data for a specific bundle.
#'
#' @param domain The domain of the bundle.
#' @param id The ID of the bundle.
#' @param version The version of the bundle. Default is "latest".
#'
#' @return A data frame with the concept data.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Usage with available_bundles, from a single row
#' smoking <- available_bundles() |> dplyr::filter(bundle_name == "Smoking")
#' concept_by_bundle(domain = smoking$domain, id = smoking$id, version = smoking$version)
#' # Using if you know the details directly
#' concept_by_bundle(domain = "observation", id = "smoking")
concept_by_bundle <- function(domain, id, version = "latest") {
  get_raw_dir(version = version, domain, "bundles", glue::glue("{id}.csv")) |>
    readr::read_csv(col_types = readr::cols(concept_id = readr::col_integer())) |>
    dplyr::mutate(domain = domain)
}
