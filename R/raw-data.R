#' @importFrom dplyr mutate
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom glue glue
#' @importFrom vroom problems


# The package will store releases internally, and not in raw data
# Until that happens, this will duplicate the bundle code

#' @title Get available raw bundles
#'
#' @description If a bundle has multiple names, then the id will be duplicated across rows
#'
#' @return dataframe that contains a "concept_name" and a "domain" column for each available concept
raw_bundles <- function() {
  raw_dir <- get_raw_dir()
  directories <- dir(raw_dir, full.names = TRUE)
  bundle_name_paths <- file.path(directories, "bundle_names.csv")

  purrr::map_dfr(bundle_name_paths, parse_bundle_names)
}


get_raw_dir <- function(..., version = "latest") {
  if (version != "latest") warning("Versioning not yet implemented, using version = 'latest'")

  file_path <- system.file("data-raw", ..., package = "omopbundles")

  if (!file.exists(file_path)) {
    path <- paste(..., sep = "/")
    stop(glue::glue("File not found in raw data, path given: {path}"))
  }

  file_path
}


parse_bundle_names <- function(bundle_name_path) {
  bundle_name <- bundle_name_path |>
    dirname() |>
    basename()

  readr::read_csv(bundle_name_path, col_types = "cc") |>
    mutate(domain = bundle_name)
}



#' @title Get concepts for a a single bundle row
#'
#' @description Retrieves concept data for a specific bundle.
#'
#' @param domain The domain of the bundle.
#' @param id The ID of the bundle.
#' @return A data frame with the concept data.
raw_concept_by_bundle <- function(domain, id) {
  file <- get_raw_dir(domain, "bundles", glue::glue("{id}.csv"))

  concepts <- file |>
    readr::read_csv(show_col_types = FALSE)

  # Check for parsing problems
  parsing_problems <- vroom::problems(concepts)
  if (nrow(parsing_problems) > 0) {
    warning(glue::glue("Warning while parsing: {file}"))
    warning(parsing_problems)
  }

  concepts |>
    dplyr::mutate(domain = domain)
}
