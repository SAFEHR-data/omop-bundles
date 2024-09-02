#' @importFrom dplyr tibble mutate
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv

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
  raw_dir <- .get_raw_dir(version = version)
  directories <- list.dirs(raw_dir, full.names = TRUE)
  domain_directories <- directories[directories != raw_dir]

  purrr::map_dfr(domain_directories, .build_concepts_from_directory) |>
    mutate(version = version)
}

.get_raw_dir <- function(version, ...) {
  if (version != "latest") warning("Versioning not yet implemented, using version = 'latest'")

  system.file("data-raw", ..., package = "omopbundles", mustWork = TRUE)
}

.build_concepts_from_directory <- function(directory) {
  concept_files <- list.files(directory)
  concept_name <- NULL

  dplyr::tibble(
    id = concept_files,
    concept_name = concept_files,
    domain = basename(directory)
  ) |>
    dplyr::mutate(concept_name = sub("\\.csv$", "", concept_name)) |>
    dplyr::mutate(concept_name = gsub("_", " ", concept_name))
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
#' concept_by_bundle(domain = "observation", id = "smoking.csv")
concept_by_bundle <- function(domain, id, version = "latest") {
  .get_raw_dir(version = version, domain, id) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::mutate(domain = domain)
}

