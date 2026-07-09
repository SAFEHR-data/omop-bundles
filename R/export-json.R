#' @importFrom jsonlite toJSON
#' @importFrom dplyr mutate select

#' @title Export bundle to Atlas concept set JSON format
#'
#' @description Exports a bundle to the Atlas concept set JSON format, which
#'   can be imported into OHDSI Atlas or other tools.
#'
#' @param bundle_id Character. Bundle ID to export
#' @param vocab_connection Vocab connection object. Created using create_vocab_connection()
#' @param include_descendants Logical. Whether to include descendants (default: TRUE)
#' @param file_path Character. Optional file path to save JSON
#'
#' @return JSON string (invisibly if file_path provided)
#' @export
#'
#' @examples
#' # Create vocab connection
#' # vocab_conn <- create_vocab_connection(
#' #   connection = db_conn,
#' #   connection_type = "OMOP"
#' # )
#'
#' # Export to JSON string
#' # json <- export_bundle_json(
#' #   bundle_id = "homeless",
#' #   vocab_connection = vocab_conn
#' # )
#'
#' # Export to file
#' # export_bundle_json(
#' #   bundle_id = "homeless",
#' #   vocab_connection = vocab_conn,
#' #   file_path = "homeless-concept-set.json"
#' # )
export_bundle_json <- function(
  bundle_id,
  vocab_connection,
  include_descendants = TRUE,
  file_path = NULL
) {
  # Get bundle concepts
  concepts <- get_bundle_concepts(
    bundle_id = bundle_id,
    vocab_connection = vocab_connection,
    include_descendants = include_descendants,
    expand_hierarchy = TRUE,
    return_metadata = TRUE
  )

  # Format as Atlas concept set JSON structure
  # Create list of items, each with a concept object
  items_list <- lapply(seq_len(nrow(concepts)), function(i) {
    concept_row <- concepts[i, ]

    # Build concept object (only include fields that exist)
    concept_obj <- list(
      CONCEPT_ID = concept_row$concept_id
    )

    # Add optional fields if they exist
    if (!is.null(concept_row$concept_name) && !is.na(concept_row$concept_name)) {
      concept_obj$CONCEPT_NAME <- concept_row$concept_name
    }
    if (!is.null(concept_row$domain_id) && !is.na(concept_row$domain_id)) {
      concept_obj$DOMAIN_ID <- concept_row$domain_id
    }
    if (!is.null(concept_row$vocabulary_id) && !is.na(concept_row$vocabulary_id)) {
      concept_obj$VOCABULARY_ID <- concept_row$vocabulary_id
    }
    if (!is.null(concept_row$concept_code) && !is.na(concept_row$concept_code)) {
      concept_obj$CONCEPT_CODE <- concept_row$concept_code
    }
    if (!is.null(concept_row$standard_concept) && !is.na(concept_row$standard_concept)) {
      concept_obj$STANDARD_CONCEPT <- concept_row$standard_concept
    }
    if (!is.null(concept_row$invalid_reason) && !is.na(concept_row$invalid_reason)) {
      concept_obj$INVALID_REASON <- concept_row$invalid_reason
    }
    if (!is.null(concept_row$valid_start_date) && !is.na(concept_row$valid_start_date)) {
      concept_obj$VALID_START_DATE <- as.character(concept_row$valid_start_date)
    }
    if (!is.null(concept_row$valid_end_date) && !is.na(concept_row$valid_end_date)) {
      concept_obj$VALID_END_DATE <- as.character(concept_row$valid_end_date)
    }

    # Determine includeDescendants
    inc_desc <- dplyr::if_else(
      is.na(concept_row$include_descendants),
      include_descendants,
      concept_row$include_descendants
    )

    list(
      concept = concept_obj,
      isExcluded = FALSE,
      includeDescendants = inc_desc,
      includeMapped = FALSE
    )
  })

  json_structure <- list(items = items_list)
  json_string <- jsonlite::toJSON(
    json_structure,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  # Early return if no file path provided
  if (is.null(file_path)) {
    return(json_string)
  }

  # Write to file
  writeLines(json_string, file_path)
  invisible(json_string)
}
