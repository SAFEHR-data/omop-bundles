tmp_raw_data_dir <- copy_mock_raw_data_files()

test_that("list_bundles returns bundles with correct columns", {
  result <- with_mocked_bindings(
    get_data_raw_dir = function() tmp_raw_data_dir,
    omopbundles::list_bundles()
  )
  expect_true(nrow(result) > 0, info = "The dataframe should not be empty")
  expect_setequal(names(result), c("bundle_id", "bundle_name", "description", "created_date", "modified_date"))
  
  # Check specific bundle exists
  parent_bundle <- dplyr::filter(result, bundle_id == "parent_bundle")
  expect_equal(nrow(parent_bundle), 1)
  expect_equal(parent_bundle$bundle_name, "Parent bundle")
})
