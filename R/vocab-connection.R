#' @importFrom dplyr filter
#' @importFrom methods is

#' @title Create vocabulary connection object
#'
#' @description Creates a vocabulary connection object that encapsulates the
#'   connection to an OMOP vocabulary database and its connection type.
#'
#' @param connection Connection object. Connection to vocabulary database
#'   (database connection, Arrow dataset, or list of data frames)
#' @param connection_type Character. Type of connection: "OMOP" or "SNOMED".
#'   Currently only "OMOP" is supported.
#'
#' @return Vocab connection object that encapsulates the connection and its type
#' @export
#'
#' @examples
#' # CDM connection for OMOP
#' # cdm <- CDMConnector::cdmFromCon(con, 
#' #   cdmName = "eunomia", 
#' #   cdmSchema = "main",
#' #   writeSchema = "main"
#' # )
#' # vocab_conn <- create_vocab_connection(
#' #   connection = cdm,
#' #   connection_type = "OMOP"
#' # )
#'
#' # Data frames for testing
#' # concept_df <- data.frame(...)
#' # ancestor_df <- data.frame(...)
#' # cdm <- list(
#' #   concept = concept_df,
#' #   concept_ancestor = ancestor_df
#' # )
#' # vocab_conn <- create_vocab_connection(
#' #   connection = cdm,
#' #   connection_type = "OMOP"
#' # )
create_vocab_connection <- function(
  connection,
  connection_type = c("OMOP", "SNOMED")
) {
  connection_type <- match.arg(connection_type)

  # Early exit for unsupported connection types
  if (connection_type == "SNOMED") {
    stop("SNOMED connection type is not yet implemented")
  }

  # Early exit for NULL connection
  if (is.null(connection)) {
    stop("Connection cannot be NULL")
  }

  # Validate OMOP connection structure
  if (connection_type == "OMOP" && is.list(connection) && !inherits(connection, "data.frame")) {
    required_tables <- c("concept", "concept_ancestor")
    missing_tables <- setdiff(required_tables, names(connection))
    if (length(missing_tables) > 0) {
      stop(
        "Missing required tables in connection: ",
        paste(missing_tables, collapse = ", ")
      )
    }
  }

  # Create vocab_connection object
  structure(
    list(
      connection = connection,
      connection_type = connection_type
    ),
    class = "vocab_connection"
  )
}

#' @title Get concept table from vocabulary connection
#'
#' @description Retrieves the concept table from a vocabulary connection.
#'
#' @param vocab_conn Vocab connection object created by create_vocab_connection()
#' @return Concept table (data frame or database table reference)
#' @noRd
get_concept_table <- function(vocab_conn) {
  # Early exit for invalid connection object
  if (!inherits(vocab_conn, "vocab_connection")) {
    stop("vocab_conn must be a vocab_connection object")
  }

  # Early exit for unsupported connection types
  if (vocab_conn$connection_type != "OMOP") {
    stop("Unsupported connection type: ", vocab_conn$connection_type)
  }

  # Handle data frame list case
  if (is.list(vocab_conn$connection) && !inherits(vocab_conn$connection, "data.frame")) {
    return(vocab_conn$connection$concept)
  }

  # For database connections, return the connection with table name
  # The caller will need to query it appropriately
  vocab_conn$connection
}

#' @title Get concept_ancestor table from vocabulary connection
#'
#' @description Retrieves the concept_ancestor table from a vocabulary connection.
#'
#' @param vocab_conn Vocab connection object created by create_vocab_connection()
#' @return Concept ancestor table (data frame or database table reference)
#' @noRd
get_concept_ancestor_table <- function(vocab_conn) {
  # Early exit for invalid connection object
  if (!inherits(vocab_conn, "vocab_connection")) {
    stop("vocab_conn must be a vocab_connection object")
  }

  # Early exit for unsupported connection types
  if (vocab_conn$connection_type != "OMOP") {
    stop("Unsupported connection type: ", vocab_conn$connection_type)
  }

  # Handle data frame list case
  if (is.list(vocab_conn$connection) && !inherits(vocab_conn$connection, "data.frame")) {
    return(vocab_conn$connection$concept_ancestor)
  }

  # For database connections, return the connection with table name
  # The caller will need to query it appropriately
  vocab_conn$connection
}

#' @title Resolve concept_code to concept_id
#'
#' @description Resolves a concept_code and vocabulary_id to a concept_id using
#'   the vocabulary connection.
#'
#' @param vocab_conn Vocab connection object
#' @param concept_code Character vector. Concept codes to resolve
#' @param vocabulary_id Character vector. Vocabulary IDs corresponding to concept codes
#' @return Data frame with concept_id, concept_code, vocabulary_id
#' @noRd
resolve_concept_code <- function(vocab_conn, concept_code, vocabulary_id) {
  # Early exit for invalid connection object
  if (!inherits(vocab_conn, "vocab_connection")) {
    stop("vocab_conn must be a vocab_connection object")
  }

  # Early exit for unsupported connection types
  if (vocab_conn$connection_type != "OMOP") {
    stop("Unsupported connection type: ", vocab_conn$connection_type)
  }

  concept_table <- get_concept_table(vocab_conn)

  # Early exit for non-data.frame connections (database connections not yet implemented)
  if (!is.data.frame(concept_table)) {
    stop("Database connection resolution not yet fully implemented")
  }

  # Handle data frame case
  concept_table |>
    dplyr::filter(
      concept_code %in% concept_code &
        vocabulary_id %in% vocabulary_id
    ) |>
    dplyr::select(.data$concept_id, .data$concept_code, .data$vocabulary_id)
}
