library(dplyr)
library(omopbundles)
library(testthat)


test_that("available_bundles isn't empty and have correct columns", {
  result <- omopbundles::available_bundles()
  expect_true(nrow(result) > 0, info = "The dataframe should not be empty")
  hiv_ab <- filter(result, concept_name == "Antibodies to HIV")
  expect_equal(hiv_ab$version, "latest")
  expect_equal(hiv_ab$id, "antibodies_to_hiv")
  expect_equal(hiv_ab$domain, "measurement")

})

test_that("Smoking exists as an observation", {
  result <- available_bundles() |>
    dplyr::filter(concept_name == "Smoking")

  expect_true(nrow(result) == 1, info = "Smoking should only exist as a single row")
  expect_equal(result$domain, "observation")
})

test_that("Concept by bundle works with character values", {
  smoking_concepts <- omopbundles::concept_by_bundle(domain = "observation", id = "smoking")
  expect_true(nrow(smoking_concepts) > 1, info = "Smoking should have multiple concepts")
  expect_false(any(is.na(smoking_concepts$concept_id)), info = "Concept ids should not be NA")
  expect_true(all(smoking_concepts$domain == "observation"), info = "Domain should be set correctly")
})

test_that("Available bundles and concept_by_bundle play nicely together", {
  smoking_bundle <- available_bundles() |>
    dplyr::filter(concept_name == "Smoking")

  smoking_concepts <- concept_by_bundle(smoking_bundle$domain, smoking_bundle$id)

  expect_true(nrow(smoking_concepts) > 1, info = "Smoking should have multiple concepts")
  expect_false(any(is.na(smoking_concepts$concept_id)), info = "Concept ids should not be NA")
  expect_true(all(smoking_concepts$domain == "observation"), info = "Domain should be set correctly")
})
