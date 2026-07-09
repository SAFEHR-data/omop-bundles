temp_raw_dir <- copy_mock_raw_data_files()
vocab_conn <- create_mock_vocab_connection()


test_that("export_bundle_json exports hierarchy, descendants, and exclusions", {
  # Given a bundle structure with a parent bundle, a descendant bundle, a non-descendant bundle, and an excluded bundle
  # When we export the parent bundle to Atlas concept set JSON
  # Then we should get JSON items for included concepts and their descendants
  # - should not have the excluded bundle concepts
  # - should not have excluded concepts or their descendants
  json <- with_mocked_bindings(
    get_data_raw_dir = function() temp_raw_dir,
    export_bundle_json(
      bundle_id = "parent_bundle",
      vocab_connection = vocab_conn
    )
  )

  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  concept_ids <- as.integer(vapply(
    parsed$items,
    function(item) item$concept$CONCEPT_ID,
    integer(1)
  ))

  # include C_PARENT_DIRECT_300 and descendants
  # include C_ANCESTOR_100 and descendants
  # include C_LEAF_200 and descendants
  # exclude C_EXCLUDED_DESC_400 and descendants
  # exclude C_EXCLUDED_CHILD_500
  expect_setequal(concept_ids, c(100, 101, 102, 200, 201, 202, 300, 301, 302))
  expect_false(any(concept_ids %in% c(400, 401, 402, 500)))

  for (item in parsed$items) {
    expect_false(item$isExcluded)
    expect_false(item$includeMapped)
    expect_setequal(names(item), c("concept", "isExcluded", "includeDescendants", "includeMapped"))
  }

  include_descendants_by_id <- stats::setNames(
    vapply(parsed$items, function(item) item$includeDescendants, logical(1)),
    concept_ids
  )
  expect_true(include_descendants_by_id[["100"]])
  expect_false(include_descendants_by_id[["200"]])
  expect_false(include_descendants_by_id[["300"]])
})
