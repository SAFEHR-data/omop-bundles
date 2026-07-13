#' @importFrom jsonlite toJSON fromJSON
#' @importFrom purrr map_dfr pmap
#' @importFrom tools file_path_sans_ext

#' @title Empty bundle concepts data frame
#' @noRd
empty_bundle_concepts_df <- function() {
  data.frame(
    bundle_id = character(0),
    concept_key = character(0),
    include_descendants = logical(0),
    is_excluded = logical(0),
    stringsAsFactors = FALSE
  )
}

#' @title Empty bundle hierarchy data frame
#' @noRd
empty_child_bundles_df <- function() {
  data.frame(
    parent_bundle_id = character(0),
    child_bundle_id = character(0),
    is_excluded = logical(0),
    stringsAsFactors = FALSE
  )
}

#' @title Empty child bundles data frame for JSON write
#' @noRd
empty_children_json_df <- function() {
  data.frame(
    child_bundle_id = character(0),
    is_excluded = logical(0),
    stringsAsFactors = FALSE
  )
}

#' @title Empty bundle concepts data frame for JSON write
#' @noRd
empty_concepts_json_df <- function() {
  data.frame(
    concept_key = character(0),
    include_descendants = logical(0),
    is_excluded = logical(0),
    stringsAsFactors = FALSE
  )
}

#' @title Get bundles JSON directory path
#' @noRd
get_bundles_dir <- function() {
  file.path(get_data_raw_dir(), "bundles")
}

#' @title Get path to a bundle JSON file
#' @noRd
bundle_json_path <- function(bundle_id) {
  file.path(get_bundles_dir(), paste0(bundle_id, ".json"))
}

#' @title List bundle IDs from JSON files in a directory
#' @noRd
bundle_ids_from_json_dir <- function(bundles_dir) {
  json_files <- list.files(bundles_dir, pattern = "\\.json$", full.names = FALSE)
  tools::file_path_sans_ext(json_files)
}

#' @title Read all bundle JSON files with a reader function
#' @noRd
read_all_bundle_json <- function(reader_fn, empty_fn) {
  bundles_dir <- get_bundles_dir()
  if (!dir.exists(bundles_dir)) {
    return(empty_fn())
  }

  ids <- bundle_ids_from_json_dir(bundles_dir)
  if (length(ids) == 0) {
    return(empty_fn())
  }

  purrr::map_dfr(ids, reader_fn)
}

#' @title Read bundle JSON file
#' @noRd
read_bundle_json_file <- function(bundle_id) {
  json_file <- bundle_json_path(bundle_id)
  if (!file.exists(json_file)) {
    return(NULL)
  }
  jsonlite::fromJSON(json_file, simplifyDataFrame = TRUE)
}

#' @title Convert concept row to JSON list element
#' @noRd
concept_row_to_json <- function(concept_key, include_descendants, is_excluded) {
  list(
    concept_key = concept_key,
    include_descendants = include_descendants,
    is_excluded = is_excluded
  )
}

#' @title Convert child bundle row to JSON list element
#' @noRd
child_bundle_row_to_json <- function(child_bundle_id, is_excluded) {
  list(
    child_bundle_id = child_bundle_id,
    is_excluded = is_excluded
  )
}

#' @title Convert data frame rows to JSON list
#' @noRd
df_to_json_list <- function(df, fn) {
  if (nrow(df) == 0) {
    return(list())
  }
  purrr::pmap(df, fn)
}

#' @title Read bundle concepts from JSON file
#'
#' @description Reads concepts for a single bundle from its JSON file.
#'
#' @param bundle_id Character. Bundle ID to read
#' @return Data frame with columns: bundle_id, concept_key, include_descendants, is_excluded
#' @noRd
read_bundle_concepts_json <- function(bundle_id) {
  json_data <- read_bundle_json_file(bundle_id)
  if (is.null(json_data)) {
    return(empty_bundle_concepts_df())
  }

  if (is.null(json_data$concepts) || length(json_data$concepts) == 0) {
    return(empty_bundle_concepts_df())
  }

  concepts_df <- json_data$concepts
  concepts_df$bundle_id <- bundle_id
  concepts_df[, c("bundle_id", "concept_key", "include_descendants", "is_excluded")]
}

#' @title Read all bundle concepts from JSON files
#'
#' @description Reads all bundle concepts from JSON files and returns a data frame
#'   in the same format as the old CSV file.
#'
#' @return Data frame with columns: bundle_id, concept_key, include_descendants, is_excluded
#' @noRd
read_all_bundle_concepts_json <- function() {
  read_all_bundle_json(read_bundle_concepts_json, empty_bundle_concepts_df)
}

#' @title Read bundle child bundles from JSON file
#'
#' @description Reads child bundles for a single bundle from its JSON file.
#'
#' @param bundle_id Character. Bundle ID to read
#' @return Data frame with columns: parent_bundle_id, child_bundle_id, is_excluded
#' @noRd
read_bundle_child_bundles_json <- function(bundle_id) {
  json_data <- read_bundle_json_file(bundle_id)
  if (is.null(json_data)) {
    return(empty_child_bundles_df())
  }

  if (is.null(json_data$child_bundles) || length(json_data$child_bundles) == 0) {
    return(empty_child_bundles_df())
  }

  child_bundles_df <- json_data$child_bundles
  child_bundles_df$parent_bundle_id <- bundle_id
  child_bundles_df[, c("parent_bundle_id", "child_bundle_id", "is_excluded")]
}

#' @title Read all bundle hierarchy from JSON files
#'
#' @description Reads all bundle hierarchy from JSON files and returns a data frame
#'   in the same format as the old CSV file (but without include_descendants).
#'
#' @return Data frame with columns: parent_bundle_id, child_bundle_id, is_excluded
#' @noRd
read_all_bundle_hierarchy_json <- function() {
  read_all_bundle_json(read_bundle_child_bundles_json, empty_child_bundles_df)
}

#' @title Write bundle concepts and child bundles to JSON file
#'
#' @description Writes concepts and child bundles for a bundle to its JSON file.
#'
#' @param bundle_id Character. Bundle ID
#' @param concepts_df Data frame. Concepts data frame with columns: concept_key, include_descendants, is_excluded
#' @param child_bundles_df Data frame. Child bundles data frame with columns: child_bundle_id, is_excluded (optional)
#' @param data_dir Character. Optional data-raw directory to write into (default: package data-raw dir)
#' @noRd
write_bundle_concepts_json <- function(bundle_id, concepts_df, child_bundles_df = NULL, data_dir = NULL) {
  target_data_dir <- if (is.null(data_dir)) get_data_raw_dir() else data_dir
  bundles_dir <- file.path(target_data_dir, "bundles")
  if (!dir.exists(bundles_dir)) {
    dir.create(bundles_dir, recursive = TRUE)
  }

  concepts_list <- df_to_json_list(concepts_df, concept_row_to_json)
  child_bundles_list <- if (is.null(child_bundles_df)) {
    list()
  } else {
    df_to_json_list(child_bundles_df, child_bundle_row_to_json)
  }

  json_structure <- list(
    concepts = concepts_list,
    child_bundles = child_bundles_list
  )

  json_string <- jsonlite::toJSON(
    json_structure,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  writeLines(json_string, file.path(bundles_dir, paste0(bundle_id, ".json")))
}

#' @title Delete bundle concepts JSON file
#'
#' @description Deletes the JSON file for a bundle.
#'
#' @param bundle_id Character. Bundle ID
#' @noRd
delete_bundle_concepts_json <- function(bundle_id) {
  json_file <- bundle_json_path(bundle_id)
  if (file.exists(json_file)) {
    file.remove(json_file)
  }
}
