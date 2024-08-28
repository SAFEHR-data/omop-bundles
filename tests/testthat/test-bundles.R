library(dplyr)
library(omopbundles)
library(testthat)


test_that("available_bundles is not empty",{
  result <- available_bundles()
  expect_true(nrow(result) > 0, info = "The dataframe should not be empty")
})

test_that("Smoking exists as an observation",{
  result <- available_bundles() |>
    dplyr::filter(concept_name == "smoking")

  expect_true(nrow(result) == 1, info = "Smoking should only exist as a single row")
  expect_equal(result$domain, "observation")
})


test_that("Avilable bundles and concept_by_bundle play nicely together",{
  smoking_bundle <- available_bundles() |>
    dplyr::filter(concept_name == "smoking")

  smoking_concepts <- concept_by_bundle(smoking_bundle)

  expect_true(nrow(smoking_concepts) > 1, info = "Smoking should have multiple concepts")
  expect_false(any(is.na(smoking_concepts$concept_id)), info = "Concept ids should not be NA")
  expect_true(all(smoking_concepts$domain == "observation"), info = "Domain should be set correctly")
})
