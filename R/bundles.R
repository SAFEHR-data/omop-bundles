#' @importFrom dplyr tibble mutate
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv

#' Get available bundles for a version
#'
#' If a bundle has multiple names, then the id will be duplicated across rows
#'
#' @param version Requested version, if not defined, the latest will be used
#' @return dataframe that contains a concept_name and a domain column for each available concept
#' @export
available_bundles <- function(version = "latest") {
  raw_dir <- .get_raw_dir()
  directories <- list.dirs(raw_dir, full.names=TRUE)
  domain_directories <- directories[directories != raw_dir]

  purrr::map_dfr(domain_directories, .build_concepts_from_directory)
}

.get_raw_dir <- function(...) {
  system.file("data", "raw", ..., package = "omopbundles", mustWork=TRUE)
}

.build_concepts_from_directory <- function(directory) {
  concept_files <- list.files(directory)

  dplyr::tibble(
    id = concept_files,
    concept_name = concept_files,
    domain = basename(directory)
  ) |>
    dplyr::mutate(concept_name = sub("\\.csv$", "", concept_name)) |>
    dplyr::mutate(concept_name = gsub("_", " ", concept_name))
}

#' Get concepts for a a single bundle row
#'
#'
#' @param bundle_row Single row of a dataframe with a domain and id
#' @return Dataframe with a concept_id and domain column
#' @export
concept_by_bundle <- function(bundle_row){
  stopifnot(is.data.frame(bundle_row))
  stopifnot(nrow(bundle_row) == 1)

  .get_raw_dir(bundle_row$domain, bundle_row$id) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::mutate(domain = bundle_row$domain)
}
