# Runs once for any test

#' Create a mock vocabulary connection reading from testdata/mock_vocab
#' 
#' @param base_path The base path to the testdata directory
#' @return A connection to the mock vocabulary
create_mock_vocab_connection <- function() {
  base_path <- testthat::test_path("testdata", "vocab")

  concept_df <- readr::read_csv(
    file.path(base_path, "concept.csv"),
    show_col_types = FALSE,
    col_types = readr::cols(
      concept_id = readr::col_integer(),
      concept_name = readr::col_character(),
      domain_id = readr::col_character(),
      vocabulary_id = readr::col_character(),
      concept_code = readr::col_character(),
      standard_concept = readr::col_character(),
      invalid_reason = readr::col_character(),
      valid_start_date = readr::col_date(),
      valid_end_date = readr::col_date()
    )
  )

  ancestor_df <- readr::read_csv(
    file.path(base_path, "concept_ancestor.csv"),
    show_col_types = FALSE,
    col_types = readr::cols(
      ancestor_concept_id = readr::col_integer(),
      descendant_concept_id = readr::col_integer(),
      min_levels_of_separation = readr::col_integer(),
      max_levels_of_separation = readr::col_integer()
    )
  )


  create_vocab_connection(
    connection = list(
      concept = concept_df,
      concept_ancestor = ancestor_df
    ),
    connection_type = "OMOP"
  )
}

#' Copy mock vocabulary files to a temporary directory
#'
#' @param mock_directory The testdata directory containing the mock vocabulary files
#' @return temporary directory containing the mock raw data files
copy_mock_raw_data_files <- function(mock_directory="raw-data") {
  # Default bundle hierarchy:
  #
  # parent_bundle
  #   - concept: C_PARENT_DIRECT_300 (include_descendants = FALSE, is_excluded = FALSE)
  # ├─(included)── desc_bundle (bundle with descendants)
  # │                ├─ concept: C_ANCESTOR_100 (include_descendants = TRUE, is_excluded = FALSE)
  # │                └─ concept: C_EXCLUDED_DESC_400 (include_descendants = TRUE, is_excluded = TRUE)
  # ├─(included)── non_desc_bundle (bundle with no descendants)
  # │                └─ concept: C_LEAF_200 (include_descendants = FALSE, is_excluded = FALSE)
  # └─(excluded)── excluded_bundle (bundle with excluded concepts)
  #                  └─ concept: C_EXCLUDED_CHILD_500 (include_descendants = FALSE, is_excluded = FALSE)
  tmp_dir <- withr::local_tempdir(.local_envir = parent.frame(2))
  file.copy(file.path(testthat::test_path("testdata", mock_directory)), tmp_dir, recursive = TRUE)

  file.path(tmp_dir, mock_directory)
}
