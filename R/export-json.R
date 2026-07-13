#' @importFrom jsonlite toJSON
#' @importFrom dplyr mutate select coalesce
#' @importFrom purrr map imap compact
#' @importFrom stats setNames

#' @title Export bundle to Atlas concept set JSON format
#'
#' @description Exports a bundle to the Atlas concept set JSON format, which
#'   can be imported into OHDSI Atlas or other tools.
#'
#' @param bundle_id Character. Bundle ID to export
#' @param vocab_connection Vocab connection object. Created using create_vocab_connection()
#' @param resolve_descendants Logical. Whether to resolve descendants (default: FALSE)
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
  resolve_descendants = TRUE,
  file_path = NULL
) {
  # Get bundle concepts
  concepts <- get_bundle_concepts(
    bundle_id = bundle_id,
    vocab_connection = vocab_connection,
    resolve_descendants = resolve_descendants,
    expand_bundle_hierarchy = TRUE,
    return_metadata = TRUE
  )

  items_list <- map(
    seq_len(nrow(concepts)),
    ~ build_atlas_item(concepts[.x, ], resolve_descendants)
  )

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


concept_field_map <- c(
  concept_name = "CONCEPT_NAME",
  domain_id = "DOMAIN_ID",
  vocabulary_id = "VOCABULARY_ID",
  concept_code = "CONCEPT_CODE",
  standard_concept = "STANDARD_CONCEPT",
  invalid_reason = "INVALID_REASON",
  valid_start_date = "VALID_START_DATE",
  valid_end_date = "VALID_END_DATE"
)

#' @title Build concept object for Atlas concept set JSON
#'
#' @description Builds a concept object for the Atlas concept set JSON format
#'
#' @param row Data frame row containing concept metadata
#'
#' @return List containing concept object
#' @noRd
build_concept_obj <- function(row) {
  optional <- imap(concept_field_map, function(json_name, col_name) {
    val <- row[[col_name]]
    if (is.na(val)) {
      return(NULL)
    }
    if (col_name %in% c("valid_start_date", "valid_end_date")) {
      val <- as.character(val)
    }
    setNames(list(val), json_name)
  })

  c(list(CONCEPT_ID = row$concept_id), compact(optional))
}

build_atlas_item <- function(row, resolve_descendants) {
  list(
    concept = build_concept_obj(row),
    isExcluded = FALSE,
    includeDescendants = coalesce(row$include_descendants, resolve_descendants),
    includeMapped = FALSE
  )
}
