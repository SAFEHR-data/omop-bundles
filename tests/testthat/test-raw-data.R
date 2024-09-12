library(dplyr)
library(omopbundles)
library(testthat)



test_that("All raw bundles have at least one name", {

})


test_that("All raw bundle names map to a bundle file that can be read", {
  bundles <- omopbundles:::raw_bundles()

  # Ensure bundles dataframe is not empty
  expect_true(nrow(bundles) > 0)

  apply(bundles, 1, function(bundle) {
    concepts <- omopbundles:::raw_concept_by_bundle(bundle["domain"], bundle["id"])

    # Check that at least one concept
    expect_true(nrow(concepts) > 0)
  })

})
