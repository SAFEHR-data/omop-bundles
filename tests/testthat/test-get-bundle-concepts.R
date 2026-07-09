# Create a temporary directory for mock bundle files
temp_raw_dir <- copy_mock_raw_data_files()
vocab_conn <- create_mock_vocab_connection()


test_that("get_bundle_concepts expands hierarchy, descendants, and exclusions", {
  # Given a bundle structure with a parent bundle, a descendant bundle, a non-descendant bundle, and an excluded bundle
  # When we get the concepts for the parent bundle
  # Then we should get the concepts for the parent bundle, the descendant bundle, the non-descendant bundle, and the excluded bundle
  # - should not have the non-descendant bundle concepts
  # - should not have the excluded bundle concepts
  concepts <- with_mocked_bindings(
    get_data_raw_dir = function() temp_raw_dir,
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
  concepts <- with_mocked_bindings(
    get_data_raw_dir = function() temp_raw_dir,
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
  concepts <- with_mocked_bindings(
    get_data_raw_dir = function() temp_raw_dir,
    get_bundle_concepts(
      bundle_id = "parent_bundle",
      vocab_connection = vocab_conn,
      expand_hierarchy = FALSE,
      return_metadata = TRUE
    )
  )

  expect_setequal(concepts$concept_id, 300)
})
