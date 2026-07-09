#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows distinct filter inner_join left_join mutate select
#' @importFrom rlang .data

#' @title Get explicit concepts for a bundle
#'
#' @description Retrieves explicit concepts for one or more bundles, optionally
#'   expanding hierarchical bundles and including descendants.
#'
#' @param bundle_id Character or character vector. Bundle ID(s) to query
#' @param vocab_connection Vocab connection object. Created using create_vocab_connection()
#' @param include_descendants Logical. Whether to include descendants
#'   (overrides bundle-level settings if TRUE)
#' @param expand_hierarchy Logical. Whether to expand child bundles (default: TRUE)
#' @param return_metadata Logical. Whether to include concept metadata (default: TRUE)
#'
#' @return Data frame with columns:
#'   - concept_id, concept_name, domain_id, vocabulary_id, concept_code,
#'   - standard_concept, invalid_reason, valid_start_date, valid_end_date
#'   - (and optionally: bundle_id, is_excluded, include_descendants)
#'
#' @export
#'
#' @examples
#' # Create vocab connection for OMOP
#' # vocab_conn <- create_vocab_connection(
#' #   connection = db_conn,
#' #   connection_type = "OMOP"
#' # )
#'
#' # Get concepts for a single bundle
#' # concepts <- get_bundle_concepts(
#' #   bundle_id = "homeless",
#' #   vocab_connection = vocab_conn,
#' #   include_descendants = TRUE
#' # )
#'
#' # Get concepts for multiple bundles
#' # concepts <- get_bundle_concepts(
#' #   bundle_id = c("homeless", "smoking"),
#' #   vocab_connection = vocab_conn
#' # )
#'
#' # Get concepts without expanding hierarchy
#' # concepts <- get_bundle_concepts(
#' #   bundle_id = "homeless_comprehensive",
#' #   vocab_connection = vocab_conn,
#' #   expand_hierarchy = FALSE
#' # )
get_bundle_concepts <- function(
  bundle_id,
  vocab_connection,
  include_descendants = NULL,
  expand_hierarchy = TRUE,
  return_metadata = TRUE
) {
  # Early exit for invalid inputs
  if (missing(bundle_id) || is.null(bundle_id) || length(bundle_id) == 0) {
    stop("bundle_id must be provided")
  }
  if (missing(vocab_connection) || is.null(vocab_connection)) {
    stop("vocab_connection must be provided")
  }
  if (!inherits(vocab_connection, "vocab_connection")) {
    stop("vocab_connection must be a vocab_connection object created by create_vocab_connection()")
  }

  # Early exit if data directory not found
  data_dir <- get_data_raw_dir()
  if (!dir.exists(data_dir)) {
    stop("data-raw directory not found. Package may need to be reinstalled.")
  }

  # Load bundle data
  bundles_file <- file.path(data_dir, "bundles.csv")
  concepts_file <- file.path(data_dir, "concepts.csv")

  bundles <- readr::read_csv(bundles_file, show_col_types = FALSE)
  concepts_df <- readr::read_csv(concepts_file, show_col_types = FALSE)
  bundle_concepts <- read_all_bundle_concepts_json()
  bundle_hierarchy <- read_all_bundle_hierarchy_json()

  # Early exit if bundles don't exist
  missing_bundles <- setdiff(bundle_id, bundles$bundle_id)
  if (length(missing_bundles) > 0) {
    stop("Bundles not found: ", paste(missing_bundles, collapse = ", "))
  }

  # Resolve hierarchy if requested
  all_bundle_ids <- if (expand_hierarchy) {
    resolve_bundle_hierarchy(bundle_id, bundle_hierarchy, bundles)
  } else {
    bundle_id
  }

  # Collect all concept_keys for the bundles
  bundle_concepts_filtered <- bundle_concepts |>
    dplyr::filter(bundle_id %in% all_bundle_ids)

  # Handle excluded concepts - separate included and excluded
  included_concepts <- bundle_concepts_filtered |>
    dplyr::filter(!.data$is_excluded)

  excluded_concepts <- bundle_concepts_filtered |>
    dplyr::filter(.data$is_excluded)

  # Join with concepts to get concept_id, concept_code, vocabulary_id
  included_with_concepts <- included_concepts |>
    dplyr::inner_join(concepts_df, by = "concept_key")

  excluded_with_concepts <- excluded_concepts |>
    dplyr::inner_join(concepts_df, by = "concept_key")

  # Resolve concept_codes to concept_ids if needed
  concepts_to_resolve <- included_with_concepts |>
    dplyr::filter(is.na(.data$concept_id) & !is.na(.data$concept_code) & !is.na(.data$vocabulary_id))

  if (nrow(concepts_to_resolve) > 0) {
    resolved <- resolve_concept_code(
      vocab_connection,
      concepts_to_resolve$concept_code,
      concepts_to_resolve$vocabulary_id
    )

    # Update concepts_df with resolved concept_ids
    for (i in seq_len(nrow(concepts_to_resolve))) {
      concept_key <- concepts_to_resolve$concept_key[i]
      matching_resolved <- resolved |>
        dplyr::filter(
          .data$concept_code == concepts_to_resolve$concept_code[i] &
            .data$vocabulary_id == concepts_to_resolve$vocabulary_id[i]
        )
      # Early continue if no match found
      if (nrow(matching_resolved) == 0) {
        next
      }
      concepts_df$concept_id[concepts_df$concept_key == concept_key] <- matching_resolved$concept_id[1]
    }

    # Re-join with updated concepts
    included_with_concepts <- included_concepts |>
      dplyr::inner_join(concepts_df, by = "concept_key")
  }

  # Determine which concepts need descendants expanded
  concepts_to_expand <- if (is.null(include_descendants)) {
    # Use bundle-level settings
    included_with_concepts |>
      dplyr::filter(include_descendants == TRUE)
  } else if (include_descendants) {
    # Expand all concepts
    included_with_concepts
  } else {
    # Don't expand any
    included_with_concepts |>
      dplyr::filter(FALSE)
  }

  # Get base concept_ids (those that don't need expansion)
  all_concept_ids <- if (nrow(concepts_to_expand) < nrow(included_with_concepts)) {
    base_concepts <- included_with_concepts |>
      dplyr::filter(!concept_key %in% concepts_to_expand$concept_key)
    unique(base_concepts$concept_id[!is.na(base_concepts$concept_id)])
  } else {
    integer(0)
  }

  # Expand descendants if needed
  if (nrow(concepts_to_expand) > 0 && vocab_connection$connection_type == "OMOP") {
    concept_ids_to_expand <- unique(concepts_to_expand$concept_id[!is.na(concepts_to_expand$concept_id)])

    if (length(concept_ids_to_expand) > 0) {
      descendant_ids <- expand_descendants(vocab_connection, concept_ids_to_expand)
      all_concept_ids <- unique(c(all_concept_ids, descendant_ids))
    }
  }

  # Early exit if no concepts to return
  if (length(all_concept_ids) == 0) {
    return(data.frame(
      concept_id = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Get concept metadata from vocab_connection
  if (!return_metadata) {
    return(data.frame(
      concept_id = all_concept_ids,
      stringsAsFactors = FALSE
    ))
  }

  concept_metadata <- get_concept_metadata(vocab_connection, all_concept_ids)

  # Early exit if no metadata found
  if (nrow(concept_metadata) == 0) {
    return(data.frame(
      concept_id = all_concept_ids,
      stringsAsFactors = FALSE
    ))
  }

  # Join with bundle information
  result <- concept_metadata |>
    dplyr::left_join(
      included_with_concepts |>
        dplyr::select("bundle_id", "concept_id", "include_descendants", "is_excluded"),
      by = "concept_id"
    )

  # Remove excluded concepts
  if (nrow(excluded_with_concepts) == 0) {
    return(dplyr::distinct(result))
  }

  excluded_ids <- unique(excluded_with_concepts$concept_id[!is.na(excluded_with_concepts$concept_id)])
  if (length(excluded_ids) == 0) {
    return(dplyr::distinct(result))
  }

  # Expand excluded descendants if needed
  excluded_to_expand <- excluded_with_concepts |>
    dplyr::filter(include_descendants == TRUE)

  if (nrow(excluded_to_expand) > 0 && vocab_connection$connection_type == "OMOP") {
    excluded_ids_to_expand <- unique(excluded_to_expand$concept_id[!is.na(excluded_to_expand$concept_id)])
    if (length(excluded_ids_to_expand) > 0) {
      excluded_descendants <- expand_descendants(vocab_connection, excluded_ids_to_expand)
      excluded_ids <- unique(c(excluded_ids, excluded_descendants))
    }
  }

  result <- result |>
    dplyr::filter(!.data$concept_id %in% excluded_ids)

  # Remove duplicates and return
  dplyr::distinct(result)
}

#' @title Resolve bundle hierarchy recursively
#'
#' @description Recursively resolves all child bundles for a given set of
#'   bundle IDs.
#'
#' @param bundle_ids Character vector. Bundle IDs to resolve
#' @param bundle_hierarchy Data frame. Bundle hierarchy relationships
#' @param bundles Data frame. All bundles
#' @return Character vector of all bundle IDs (including children)
#' @keywords internal
resolve_bundle_hierarchy <- function(bundle_ids, bundle_hierarchy, bundles) {
  all_bundle_ids <- bundle_ids
  to_process <- bundle_ids

  while (length(to_process) > 0) {
    # Find child bundles
    children <- bundle_hierarchy |>
      dplyr::filter(.data$parent_bundle_id %in% to_process & !.data$is_excluded) |>
      dplyr::pull(.data$child_bundle_id)

    # Add new children
    new_children <- setdiff(children, all_bundle_ids)
    all_bundle_ids <- c(all_bundle_ids, new_children)
    to_process <- new_children
  }

  unique(all_bundle_ids)
}

#' @title Expand concept descendants
#'
#' @description Expands a set of concept IDs to include all descendant concepts
#'   using the concept_ancestor table.
#'
#' @param vocab_conn Vocab connection object
#' @param concept_ids Integer vector. Concept IDs to expand
#' @return Integer vector of all concept IDs (including descendants)
#' @keywords internal
expand_descendants <- function(vocab_conn, concept_ids) {
  # Early exit for unsupported connection types
  if (vocab_conn$connection_type != "OMOP") {
    stop("Descendant expansion only supported for OMOP connections")
  }

  ancestor_table <- get_concept_ancestor_table(vocab_conn)

  # Early exit for non-data.frame connections
  if (!is.data.frame(ancestor_table)) {
    stop("Database connection descendant expansion not yet fully implemented")
  }

  # Handle data frame case
  descendants <- ancestor_table |>
    dplyr::filter(.data$ancestor_concept_id %in% concept_ids) |>
    dplyr::pull(.data$descendant_concept_id) |>
    unique()

  # Include original concepts
  unique(c(concept_ids, descendants))
}

#' @title Get concept metadata from vocabulary connection
#'
#' @description Retrieves concept metadata (name, domain, etc.) for a set of
#'   concept IDs.
#'
#' @param vocab_conn Vocab connection object
#' @param concept_ids Integer vector. Concept IDs to retrieve
#' @return Data frame with concept metadata
#' @keywords internal
get_concept_metadata <- function(vocab_conn, concept_ids) {
  concept_table <- get_concept_table(vocab_conn)

  # Early exit for non-data.frame connections
  if (!is.data.frame(concept_table)) {
    stop("Database connection metadata retrieval not yet fully implemented")
  }

  # Handle data frame case
  concept_table |>
    dplyr::filter(.data$concept_id %in% concept_ids) |>
    dplyr::select(
      "concept_id",
      "concept_name",
      "domain_id",
      "vocabulary_id",
      "concept_code",
      "standard_concept",
      "invalid_reason",
      "valid_start_date",
      "valid_end_date"
    ) |>
    dplyr::distinct(.data$concept_id, .keep_all = TRUE)
}
