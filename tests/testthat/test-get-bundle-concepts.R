create_mock_vocab_connection <- function() {
  # Bundle hierarchy for these tests:
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
  base_path <- testthat::test_path("testdata", "mock_vocab")

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

write_mock_bundle_files <- function(bundle_dir) {
  dir.create(file.path(bundle_dir, "bundles"), recursive = TRUE, showWarnings = FALSE)

  bundles_df <- data.frame(
    bundle_id = c("parent_bundle", "desc_bundle", "non_desc_bundle", "excluded_bundle"),
    bundle_name = c("Parent bundle", "Desc bundle", "Non descendant bundle", "Excluded bundle"),
    description = NA_character_,
    created_date = as.Date("2024-01-01"),
    modified_date = as.Date("2024-01-01"),
    stringsAsFactors = FALSE
  )
  write.csv(bundles_df, file.path(bundle_dir, "bundles.csv"), row.names = FALSE)

  concepts_df <- data.frame(
    concept_key = c(
      "C_PARENT_DIRECT_300",
      "C_ANCESTOR_100",
      "C_LEAF_200",
      "C_EXCLUDED_DESC_400",
      "C_EXCLUDED_CHILD_500"
    ),
    concept_id = c(300, 100, 200, 400, 500),
    concept_code = c("300", "100", "200", "400", "500"),
    vocabulary_id = rep("TEST", 5),
    stringsAsFactors = FALSE
  )
  write.csv(concepts_df, file.path(bundle_dir, "concepts.csv"), row.names = FALSE)

  write_bundle_json <- function(bundle_id, concepts, child_bundles = list()) {
    jsonlite::write_json(
      list(
        concepts = concepts,
        child_bundles = child_bundles
      ),
      file.path(bundle_dir, "bundles", paste0(bundle_id, ".json")),
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }

  write_bundle_json(
    "parent_bundle",
    concepts = list(
      list(
        concept_key = "C_PARENT_DIRECT_300",
        include_descendants = FALSE,
        is_excluded = FALSE
      )
    ),
    child_bundles = list(
      list(child_bundle_id = "desc_bundle", is_excluded = FALSE),
      list(child_bundle_id = "non_desc_bundle", is_excluded = FALSE),
      list(child_bundle_id = "excluded_bundle", is_excluded = TRUE)
    )
  )

  write_bundle_json(
    "desc_bundle",
    concepts = list(
      list(
        concept_key = "C_ANCESTOR_100",
        include_descendants = TRUE,
        is_excluded = FALSE
      ),
      list(
        concept_key = "C_EXCLUDED_DESC_400",
        include_descendants = TRUE,
        is_excluded = TRUE
      )
    )
  )

  write_bundle_json(
    "non_desc_bundle",
    concepts = list(
      list(
        concept_key = "C_LEAF_200",
        include_descendants = FALSE,
        is_excluded = FALSE
      )
    )
  )

  write_bundle_json(
    "excluded_bundle",
    concepts = list(
      list(
        concept_key = "C_EXCLUDED_CHILD_500",
        include_descendants = FALSE,
        is_excluded = FALSE
      )
    )
  )
}

test_that("get_bundle_concepts expands hierarchy, descendants, and exclusions", {
  # Given a bundle structure with a parent bundle, a descendant bundle, a non-descendant bundle, and an excluded bundle
  # When we get the concepts for the parent bundle
  # Then we should get the concepts for the parent bundle, the descendant bundle, the non-descendant bundle, and the excluded bundle
  # - should not have the non-descendant bundle concepts
  # - should not have the excluded bundle concepts
  bundle_dir <- withr::local_tempdir()
  write_mock_bundle_files(bundle_dir)
  vocab_conn <- create_mock_vocab_connection()

  concepts <- with_mocked_bindings(
    get_data_raw_dir = function() bundle_dir,
    get_bundle_concepts(
      bundle_id = "parent_bundle",
      vocab_connection = vocab_conn,
      return_metadata = TRUE
    )
  )
  # include C_PARENT_DIRECT_300 but no descendants
  # include C_ANCESTOR_100 and descendants
  # include C_LEAF_200 but not descendants
  # exclude C_EXCLUDED_DESC_400 and descendants
  # exclude C_EXCLUDED_CHILD_500
  expect_setequal(concepts$concept_id, c(100, 101, 102, 200, 300))
  # exclude 400 and descendants (401 and 402)
  expect_false(any(concepts$concept_id %in% c(301, 400, 401, 500)))
})

test_that("include_descendants = FALSE overrides bundle defaults", {
  bundle_dir <- withr::local_tempdir()
  write_mock_bundle_files(bundle_dir)
  vocab_conn <- create_mock_vocab_connection()

  concepts <- with_mocked_bindings(
    get_data_raw_dir = function() bundle_dir,
    get_bundle_concepts(
      bundle_id = "desc_bundle",
      vocab_connection = vocab_conn,
      include_descendants = FALSE,
      expand_hierarchy = FALSE,
      return_metadata = TRUE
    )
  )

  expect_setequal(concepts$concept_id, 100)
  expect_false(any(concepts$concept_id %in% c(101, 102)))
})

test_that("expand_hierarchy = FALSE limits to the requested bundle", {
  bundle_dir <- withr::local_tempdir()
  write_mock_bundle_files(bundle_dir)
  vocab_conn <- create_mock_vocab_connection()

  concepts <- with_mocked_bindings(
    get_data_raw_dir = function() bundle_dir,
    get_bundle_concepts(
      bundle_id = "parent_bundle",
      vocab_connection = vocab_conn,
      expand_hierarchy = FALSE,
      return_metadata = TRUE
    )
  )

  expect_setequal(concepts$concept_id, 300)
})
